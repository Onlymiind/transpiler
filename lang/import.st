module exaple;

type Foo u32;

type Iface interface {
    Foo(u32, u32) bool;
    Bar(*u32, const u64) u16;
}

type Bar struct {
    x: u32;
    y: u32;
}

type En enum : u8 {
    Foo = 1;
    Bar = 10;
    Bars = 20;
    Count = 3;
}

type Variant union<u8, u16, u32>;

type Tup tuple<u8, u16, u32>;

type Alias = u32;

type Result<T: Iface, E> union<T, E>;

func (b *Bar) Foo(lhs: u32, rhs: u32) bool {
    return  lhs == rhs;
}

func Bar(b: ?*Bar, ptr: *u32, constant: const u64) u16 {
    if b == nil {
        return 0;
    }
    *ptr = (b.x * b.y + *ptr) / u32(constant);
    return u16(*ptr);
}

