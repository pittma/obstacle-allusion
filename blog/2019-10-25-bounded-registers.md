---
title: "Using Type-Level Programming in Rust to Make Safer Hardware Abstractions"
publish: true
track: true
home: true
tags: programming, rust, formal methods
origin: '<a href="https://blog.auxon.io/2019/10/25/type-level-registers/">auxon</a>'
updated: 04-02-2023
---

Introducing
[`bounded-registers`](https://github.com/auxoncorp/bounded-registers).

In the world of systems programming where one may find themselves
writing hardware drivers or interacting directly with memory-mapped
devices, that interaction is almost always through memory-mapped
registers provided by the hardware. We typically interact with these
things through bitwise operations on some fixed-width numeric type.

For instance, imagine an 8-bit register with three fields:

```
+----------+------+-----------+---------+
| (unused) | Kind | Interrupt | Enabled |
+----------+------+-----------+---------+
   5-7       2-4        1          0
```

The number below the field name prescribes the bits used by that field
in the register. To enable this register, one would write the value
`1`, represented in binary as `0000_0001`, to set the enabled field’s
bit. Often, though, we also have an existing configuration in the
register that we don’t want to disturb. Say we want to enable
interrupts on our device above but also want to be sure that the
device itself remains enabled. To do that, we must combine the
Interrupt field’s value with the `Enabled` field’s value. We’d do that
with bitwise operations:

```
1 | (1 << 1)
```

This gives us the binary value `0000_0011` by `or`-ing 1 with 2, which
we get by shifting 1 left by 1. We can write this to our register,
leaving it enabled but also enabling interrupts.

This is a lot to keep in our heads, especially when dealing with
potentially 100s of registers for a complete system. In practice, we
do this with mnemonics which track a field’s position in a register
and how wide that field is—i.e. what’s its _upper bound_?

Here’s an example of one of these mnemonics, they’re C macros that
replace their occurrences with the code on the right-hand side. This
is our shorthand for the register we laid out above. The left-hand
side of the `&` puts us in position for that field and the right-hand
side limits us to only that field’s bits.

```c
#define REG_ENABLED_FIELD(x) (x << 0) & 1
#define REG_INTERRUPT_FIELD(x) (x << 1) & 2
#define REG_KIND_FIELD(x) (x << 2) & (7 << 2)
```

We’d then use these to abstract over the derivation of a register’s
value with something like:

```c
void set_reg_val(reg* u8, val u8);

fn enable_reg_with_interrupt(reg* u8) {
    set_reg_val(reg, REG_ENABLED_FIELD(1) | REG_INTERRUPT_FIELD(1));
}
```

And this is the state of the art, really. In fact, this is how the
bulk of the drivers appear in the Linux kernel.

**Is there a better way?** Consider the boon to safety and expressibility
if our type system was one borne out of modern programming languages
research. That is, what could we do with a richer, more expressive
type system to make this process safer and more tenable?

# Level I: Using Rust to talk to hardware

With Rust we can use data structures to represent fields, we can
attach them to specific registers, and we can provide concise and
sensible ergonomics with interacting with the hardware. This first
example uses the most basic facilities provided by Rust, but
regardless, just the added structure alleviates some of the density
from the C example above. Now, a field is a named thing, not a number
derived from shadowy bitwise operators, and registers are types with
state—one extra layer of abstraction over the hardware.

Continuing with our register above as an example:

```
+----------+------+-----------+---------+
| (unused) | Kind | Interrupt | Enabled |
+----------+------+-----------+---------+
   5-7       2-4        1          0
```

How might we want to express such a thing in Rust types?

We’ll start in a similar way, by defining constants for each field’s
_offset_—that is, how far it is from the least significant bit—and its
mask. A _mask_ is a value whose binary representation can be used to
update or read the field from inside the register.

```rust
const ENABLED_MASK: u8 = 1;
const ENABLED_OFFSET: u8 = 0;

const INTERRUPT_MASK: u8 = 2;
const INTERRUPT_OFFSET: u8 = 1;

const KIND_MASK: u8 = 7 << 2;
const KIND_OFFSET: u8 = 2;
```

Next, we’ll declare a field type and do our operations to convert a
given value into its position-relevant value for use inside the
register.

```rust
struct Field {
    value: u8,
}

impl Field {
    fn new(mask: u8, offset: u8, val: u8) -> Self {
        Field {
            value: (val << offset) & mask,
        }
    }
}
```

Finally, a `Register` type, which wraps around the numeric type that
matches the width of our register. `Register` has an update function
which updates the register with the given field.

```rust
struct Register(u8);

impl Register {
    fn update(&mut self, val: Field) {
        self.0 = self.0 | field.value;
    }
}

fn enable_register(&mut reg) {
    reg.update(Field::new(ENABLED_MASK, ENABLED_OFFSET, 1));
}
```

This is nice, but it’s not ideal. We have to remember to bring the
mask and offset with us, and we’re calculating them ad hoc, by hand,
which could be problematic. We humans aren’t great at precise and
repetitive tasks—we tend to get tired or lose focus and this leads to
mistakes. Transcribing by hand the masks and offsets one register at a
time will almost certainly end badly. This is the kind of task we
ought to leave to a machine.

Secondarily, what if there was a way to have the field’s type itself
carry the mask and offset information? What if we could catch mistakes
in our implementation for how we access and interact with hardware
registers at compile-time instead of discovering them at runtime?
Perhaps we can lean on one of the strategies commonly used to suss out
issues at compile-time, like types.

# Leveling Up

Let’s modify our earlier example by using
[typenum](https://docs.rs/crate/typenum), a library that provides
numbers and arithmetic at the type level. Here we’ll parameterize our
`Field` type with its mask and offset, making it available for any
instance of `Field` without needing to include it at the callsite.

```rust
#[macro_use]
extern crate typenum;

use core::marker::PhantomData;

use typenum::*;

// Now we’ll add Mask and Offset to Field’s type
struct Field<Mask: Unsigned, Offset: Unsigned> {
    value: u8,
    _mask: PhantomData<Mask>,
    _offset: PhantomData<Offset>,
}

// We can use type aliases to give meaningful names to
// our fields (and not have to remember their offsets masks).
type RegEnabled = Field<U1, U0>;
type RegInterrupt = Field<U2, U1>;
type RegKind = Field<op!(U7 << U2), U2>;
```

And now, when revisiting `Field`’s constructor, we can elide the mask
and offset parameters, as the type itself contains that information:

```rust
impl<Mask: Unsigned, Offset: Unsigned> Field<Mask, Offset> {
    fn new(val: u8) -> Self {
        Field {
            value: (val << Offset::U8) & Mask::U8,
            _mask: PhantomData,
            _offset: PhantomData,
        }
    }
}

// And to enable our register...
fn enable_register(&mut reg) {
    reg.update(RegEnabled::new(1));
}
```

This is looking pretty good, but… what happens when we make a mistake
regarding whether a given value will fit into a field? Consider a
simple typo where we put `10` instead of `1`:

```rust
fn enable_register(&mut reg) {
    reg.update(RegEnabled::new(10));
}
```

In our code above? What is the expected outcome? Well, the code we
have now will set that enabled bit to 0 because `10 & 1 = 0`. That’s
unfortunate; it would be nice to know that a value I’m attempting to
write into a field will actually fit into that field before attempting
a write. As a matter of fact, I’d consider lopping off the high bits
of an errant field value _undefined behavior_ (\*gasps\*).

# Safety First

How can we check that a field’s value fits in its prescribed position
in a general way? More type-level numbers!

We can add a `Width` parameter to `Field`, and use it to verify that
we can fit the given value into the field.

```rust
struct Field<Width: Unsigned, Mask: Unsigned, Offset: Unsigned> {
    value: u8,
    _mask: PhantomData<Mask>,
    _offset: PhantomData<Offset>,
    _width: PhantomData<Width>,
}

type RegEnabled = Field<U1,U1, U0>;
type RegInterrupt = Field<U1, U2, U1>;
type RegKind = Field<U3, op!(U7 << U2), U2>;

impl<Width: Unsigned, Mask: Unsigned, Offset: Unsigned> Field<Width, Mask, Offset> {
    fn new(val: u8) -> Option<Self> {
        if val <= (1 << Width::U8) - 1 {
            Some(Field {
                value: (val << Offset::U8) & Mask::U8,
                _mask: PhantomData,
                _offset: PhantomData,
                _width: PhantomData,
            })
        } else {
            None
        }
    }
}
```

Now we can only construct a `Field` if the given value fits! Otherwise,
we have `None`, which would signal to us that an error has occurred,
rather than lop off the high bits of the value and silently write an
unexpected value.

Note, though, this will raise an error at runtime. However, we knew
the value we wanted to write beforehand, remember? Let’s use that fact
to raise the assurance bar. We can teach the compiler to reject
entirely a program which has an invalid field value—we don’t have to
wait until we run it!

This time we’ll add a trait bound (the `where` clause) to a new
realization of new, called `new_checked` that asks that the incoming
value be less-than-or-equal-to the maximum possible value a field with
the given `Width` can hold.

```rust
struct Field<Width: Unsigned, Mask: Unsigned, Offset: Unsigned> {
    value: u8,
    _mask: PhantomData<Mask>,
    _offset: PhantomData<Offset>,
    _width: PhantomData<Width>,
}

type RegEnabled = Field<U1, U1, U0>;
type RegInterrupt = Field<U1, U2, U1>;
type RegKind = Field<U3, op!(U7 << U2), U2>;

impl<Width: Unsigned, Mask: Unsigned, Offset: Unsigned> Field<Width, Mask, Offset> {
    const fn new_checked<V: Unsigned>() -> Self
    where
        V: IsLessOrEqual<op!((U1 << Width) - U1), Output = True>,
    {
        Field {
            value: (V::U8 << Offset::U8) & Mask::U8,
            _mask: PhantomData,
            _offset: PhantomData,
            _width: PhantomData,
        }
    }
}
```

Only numbers which for which this property holds have an
implementation of this trait, so if we give a number which does not
fit, it’ll fail to compile. Take a look!

```rust
fn enable_register(&mut reg) {
    reg.update(RegEnabled::new_checked::<U10>());
}
```

```
12 |     reg.update(RegEnabled::new_checked::<U10>());
   |                           ^^^^^^^^^^^^^^^^ expected struct `typenum::B0`, found struct `typenum::B1`
   |
   = note: expected type `typenum::B0`
           found type `typenum::B1`
```

Now we’ve really done it. `new_checked` will result in the failure to
produce a program which has an errant too-high value for a field. So
our typo from before won’t blow up at runtime, as we would never have
gotten an artifact to run. Now we’re nearing the Peak Rust Vicinity
with regard to how safe we can make memory-mapped hardware
interactions.

However, what we wrote down way back in the first example in C was far
more succinct than the type parameter salad we ended up with. Is doing
such a thing even tractable when you’re talking about potentially
hundreds or even thousands of registers?

# Safe and Accessible

Earlier I called out calculating our masks by hand as being
problematic, but then all I really did was the same problematic
thing—albeit at the type-level. While the use of such an approach is
nice, getting to the point that we can actually write any code
requires quite a bit of boilerplate and manual transcription (I’m
talking about the type synonyms here).

We wanted something like the [TockOS mmio
registers](https://docs.rs/tock-registers/0.3.0/tock_registers/), but
one that would generate our typesafe implementations with the least
amount of manual transcription possible. What came out of this was a
macro which generates the necessary boilerplate to get a Tock-like API
plus our type-based bounds checking. To use it, you write down some
information about a register, its fields, their width and offsets, and
optional enum-like values should you want to give “meaning” to the
possible values a field can have.

```rust
register! {
    // The register's name
    Status,
    // The type which represents the whole register.
    u8,
    // The register's mode, ReadOnly, ReadWrite, or WriteOnly.
    RW,
    // And the fields in this register.
    Fields [
        On    WIDTH(U1) OFFSET(U0),
        Dead  WIDTH(U1) OFFSET(U1),
        Color WIDTH(U3) OFFSET(U2) [
            Red    = U1,
            Blue   = U2,
            Green  = U3,
            Yellow = U4
        ]
    ]
}
```

From this, we generate register and field types like the final example
above where the indices—the `Width`, `Mask`, and `Offset`—are derived
from the values input in the `WIDTH` and `OFFSET` sections of a
field’s definitions. Also, notice that the numbers here are all
`typenums`; they’re going to go directly into our `Field` definitions!

The generated code namespaces registers and their fields through the
name given for the register and the fields, something like this:

```rust
mod Status {
    struct Register(u8);
    mod On {
        struct Field; // There is of course more to this definition
    }
    mod Dead {
        struct Field;
    }
    mod Color {
        struct Field;
        pub const Red: Field = Field::<U1>new();
        // &c.
    }
}
```

The generated API contains the nominally expected read and write
primitives to get at the raw register value, but it also has ways to
get a single field’s value, do collective actions, and find out if any
or all of a collection of bits are set. You can read the documentation
on the [complete generated API
here](https://github.com/auxoncorp/bounded-registers#the-register-api).

# Kicking the Tires

So what does it look like to actually use these definitions for a real
device? Is code going to be littered with type parameters, obscuring
any real logic from view?

No! By using type synonyms and type inference, we effectively never
have to think about the type-level part of the program at all. We get
to interact with the hardware in a straightforward way and get those
bounds-related assurances automatically.

Here’s an example of a UART register block. We’ll skip the declaration
of the registers themselves, as that would be too much to include
here. Instead, we’ll start with a register “block” then help the
compiler know how to look these registers up from a pointer to the
head of the block. We do that by implementing `Deref` and `DerefMut`.

```rust
#[repr(C)]
pub struct UartBlock {
    rx: UartRX::Register,
    _padding1: [u32; 15],
    tx: UartTX::Register,
    _padding2: [u32; 15],
    control1: UartControl1::Register,
}

pub struct Regs {
    addr: usize,
}

impl Deref for Regs {
    type Target = UartBlock;

    fn deref(&self) -> &UartBlock {
        unsafe { &*(self.addr as *const UartBlock) }
    }
}

impl DerefMut for Regs {
    fn deref_mut(&mut self) -> &mut UartBlock {
        unsafe { &mut *(self.addr as *mut UartBlock) }
    }
}
```

Once this is in place, you’ll find that the use of these registers is
as simple as `read()` and `modify()`.

```rust
fn main() {
    // A pretend register block.
    let mut x = [0_u32; 33];

    let mut regs = Regs {
        // Some shenanigans to get at `x` as though it were a
        // pointer. Normally you'd be given some address like
        // `0xDEADBEEF` over which you'd instantiate a `Regs`.
        addr: &mut x as *mut [u32; 33] as usize,
    };

    assert_eq!(regs.rx.read(), 0);

    regs.control1
        .modify(UartControl1::Enable::Set + UartControl1::RecvReadyInterrupt::Set);

    // The first bit and the 10th bit should be set.
    assert_eq!(regs.control1.read(), 0b_10_0000_0001);
}
```

For fields whose values we don’t know ahead of time, we can build
one[^field]. I’m using unwrap here, but in a real program with unknown inputs,
you’d probably want to check that you got a `Some` back from that new
call[^some].

[^field]: `get_field` looks a little weird. I’m looking at the `Field::Read`
    part, specifically. `Field` is a type, and we need an instance of that
    type to pass to `get_field`. A cleaner API maybe something like:
    
    ```rust
    regs.rx.get_field::<UartRx::Data::Field>();
    ```
    
    But remember that Field is a type synonym which has fixed indices for
    width, offset, &c. To be able to parameterize get_field like this,
    we’d need higher-kinded types!

[^some]: Technically, a read from a register field is by definition
    only going to give you a value within the prescribed bounds, but
    we don’t live in a pure world and you never know what’s going to
    happen when external systems come into play. We’re at the behest
    of the Hardware Gods here, so instead of forcing you into a “might
    panic” situation, we give you back the `Option` to handle a This
    Should Never Happen case.
	
```rust
fn main() {
    // A pretend register block.
    let mut x = [0_u32; 33];

    let mut regs = Regs {
        // Some shenanigans to get at `x` as though it were a
        // pointer. Normally you'd be given some address like
        // `0xDEADBEEF` over which you'd instantiate a `Regs`.
        addr: &mut x as *mut [u32; 33] as usize,
    };

    let input = regs.rx.get_field(UartRX::Data::Field::Read).unwrap();
    regs.tx.modify(UartTX::Data::Field::new(input).unwrap());
}
```


# Decoding Failure

Depending on your personal pain threshold you may have noticed that
the errors are nearly unintelligible. Let’s look at a not-so-subtle
reminder of what I’m talking about:

```
error[E0271]: type mismatch resolving `<typenum::UInt<typenum::UInt<typenum::UInt<typenum::UInt<typenum::UInt<typenum::UTerm, typenum::B1>, typenum::B0>, typenum::B1>, typenum::B0>, typenum::B0> as typenum::IsLessOrEqual<typenum::UInt<typenum::UInt<typenum::UInt<typenum::UInt<typenum::UTerm, typenum::B1>, typenum::B0>, typenum::B1>, typenum::B0>>>::Output == typenum::B1`
  --> src/main.rs:12:5
   |
12 |     less_than_ten::<U20>();
   |     ^^^^^^^^^^^^^^^^^^^^ expected struct `typenum::B0`, found struct `typenum::B1`
   |
   = note: expected type `typenum::B0`
       found type `typenum::B1`
```

The expected `typenum::B0` found `typenum::B1` part kind of makes
sense, but what on Earth is the `typenum::UInt<typenum::UInt,
typenum::UInt`... nonsense? Well, `typenum` represents numbers as
binary cons cells! Errors like this make it hard, especially when you
have several of these type-level numbers confined to tight quarters,
to know which number it’s even talking about. Unless, of course, it’s
second nature for you to translate baroque binary representations to
decimal ones.

After the `U100th` time attempting to decipher any meaning from this
mess, a teammate got Mad As Hell And Wasn’t Going To Take It Anymore
and made a little utility,
[`tnfilt`](https://github.com/auxoncorp/tnfilt), to parse the meaning
out from the misery that is namespaced binary cons cells. `tnfilt`
takes the cons cell-style notation and replaces it with sensible
decimal numbers. So, along with bounded-registers we’d like to also
share `tnfilt`. You use it like this:

```
$ cargo build 2>&1 | tnfilt
```

It transforms the output above into something like this:

```
error[E0271]: type mismatch resolving `<U20 as typenum::IsLessOrEqual<U10>>::Output == typenum::B1`
```

Now that makes sense!

# In Conclusion

Memory-mapped registers are used ubiquitously when interacting with
hardware from our software, and there are myriad ways in which to
portray those interactions, each of which has a different place on the
spectra of ease-of-use and safety. With `bounded-registers`, we
started by locking ourselves in place right at the edge of the
more-safe side of that safety spectrum and then tried to figure out
how to move the ease-of-use slider closer to the easy end. From those
ambitions `bounded-registers` was born, and I hope you'll keep it in
mind any time you encounter memory-mapped devices in you adventures.
