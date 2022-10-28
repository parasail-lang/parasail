These sources are a preliminary version of examples/extend_tree.psl
containing the following two semantic errors:

interface Binary<> extends Expr<> is
    type Binop is Enum<[#plus, #minus, #times, #divide, #pow]>;
    func Image(XX : Binop) -> Univ_String;       // Test: Forcing amiguity
    func Image(Op : Binop) -> Univ_String;
    func Eval2(B : Binary) -> Univ_Real;
    abstract func Eval(B : Binary) -> Univ_Real; // Test: Method not implemented
    func Display(B : Binary; Indent : Univ_Integer := 0);
    func Create(Op : Binop; Left, Right : Expr+) -> Binary;
    func Count(T : Binary) -> Univ_Integer;
end interface Binary;

The function Image is ambiguous (and it is detected by the translator
in a call to this function).

The function Eval is not implemented in "Class Binary"
