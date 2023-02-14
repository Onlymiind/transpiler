var dfhsf : union<Foo, Bar, u32, error>;

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

func Foo(b: *Bar, lhs: u32, rhs: u32) bool {
    return  lhs == rhs;
}

func Bar(b: ?*Bar, ptr: *u32, constant: u64) u16 {
    if b == nil {
        return 0;
    }
    *ptr = (b.x * b.y + *ptr) / u32(constant);
    return u16(*ptr);
}

// comment, comment \n


func foo(x: i32, y: u32) u64 {
    return u64(y + u32(x));
}

func errors(option : bool) union<u32, error> {
    if option {
        return err();
    }
    return 10;
}

func main() ?error {

    _ = foo(1000, 999);

    var x: bool;
    var bar := example::Bar{y = 10, x = 111};

    x = bar.Foo(1984, 1984);

    var works : u32 = { errors(false); } ? [res : u32]{ res; }, [err : error]{ return err; };

    var returns : u32;
    // do something with returns
    returns = { errors(true); } ? [res : u32]{ res; }, [err : error]{ return err; };

    if x {
        var y: *u32 = new<u32>(10);
        defer delete(y);

        std::println(bar.Bar(y, u64(*y)));
    }



    return nil;
}

func unnamed(u64, error) {}

type Functor struct {
    data: *u8;
    fun: func (*u8, u16) ?u32;
}