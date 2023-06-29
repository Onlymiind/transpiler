type Foo struct {
    x: u8;
    y: u8;
}

type Bar struct {
    x: Foo;
    y: u8;
    z: func(u8, u8) u8;
    i: struct {
        x: u8;
        y: func(u8);
    };
}

type Barz Bar;

type Foo Bar;

type Bar u8;

func foo(arg0 : u8, arg1 : u8, arg2 : Foo) Barz {
    "dassj;aijfdk\n\n\t";
    't';
    '\t';
}
