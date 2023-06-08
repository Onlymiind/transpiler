type Foo struct {
    x: u8;
    y: u8;
}

type Bar struct {
    x: Foo;
    y: u8;
}

type Barz Bar;

func foo(arg0 : u8, arg1 : u8, arg2 : Foo) {}
