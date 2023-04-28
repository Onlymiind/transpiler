func example() {
    a + b * y - x + z * 100;
}

func unnamed(u64, error) {}

type Functor struct {
    data: *u8;
    fun: func (*u8, u16) ?u32;
}


type Erroneous struct {
    field1 : u8;
    sddffwdfsfs
    field2 : u16;
    field3 : u16;
}

var dfhsf : union<Foo, Bar, u32, error> = 10 + 11 * 180 - 1;

type Foo u32;

type Foo324 roieuwr;

//type Iface interface {
//    Foo(u32, u32) bool;
//    Bar(*u32, const u64) u16;
//}

type Bar struct {
    x: ?***?*?*?u32;
    y: u32;
}

type Barz<T, U, V> struct {
    abc : a_type<T, u32>;
    un : U;
    dask : Tup<V, V, V>;
}

//type En enum<u8> {
//    Foo = 1;
//    Bar = 10;
//    Bars = 20;
//    Count = 3;
//}

type Variant union<u8, u16, u32>;

type Tup<T, U, V> tuple<u8, u16, u32, T, U, V>;

type Result<T, E> union<T, E>;