with  Psc.messages;
with  Psc.trees;
with  Psc.strings;
with  Psc.source_Positions;
with  Psc.trees.lists;
with  Psc.trees.param_Decl;
with  Psc.trees.unary;
with  Psc.trees.binary;
with  Psc.trees.assign_Stmt;
with  Psc.trees.control_Stmt;
use   Psc;
use   Psc.trees;
pragma Style_Checks (Off);
package Parython_Tokens is

 
    type yystype_enum is (
      One_Token, One_Tree, One_List, Optional, 
      Construct_Qualifier,
      Formals_And_Interfaces,
      Two_Lists,
      Param_Mode,
      One_Unary_Op,
      One_Binary_Op,
      One_Assign_Op,
      Construct_Kind,
      Optional_End_Token);

    type yystype(Kind : yystype_enum := One_Tree) is record
      case Kind is
	when One_Tree =>
	  Tree : Optional_Tree;
	when One_List =>
	  List : Lists.List;
	when Optional =>
	  Is_Present : Boolean := False;
	when Formals_And_Interfaces =>
	  Has_Module_Formals : Boolean := False;
	  Module_Formals : Lists.List;
	  Extends : Optional_Tree;
	  Implements : Lists.List;
        when Two_Lists =>
	  First_List : Lists.List;
	  Second_List : Lists.List;
	when Param_Mode =>
	  Param_Kind : Param_Decl.Param_Kind;
	  Param_Locking : Param_Decl.Param_Locking;
	when One_Token | Construct_Qualifier |
           One_Unary_Op | One_Binary_Op | One_Assign_Op =>
	  Source_Pos : Source_Positions.Source_Position;
	  case Kind is
	    when One_Token =>
	      Str : PSC.Strings.U_String;
            when Construct_Qualifier =>
              Is_Abstract : Boolean := False;
              Is_Private : Boolean := False;
              Is_Concurrent : Boolean := False;
              Is_Optional : Boolean := False;
              Is_While : Boolean := False;
              Is_Until : Boolean := False;
              Is_Ref : Boolean := False;
              Is_Global : Boolean := False;
              Is_Queued : Boolean := False;
	    when One_Unary_Op =>
	      Unary_Op : Unary.Unary_Operator_Enum;
	    when One_Binary_Op =>
	      Binary_Op : Binary.Binary_Operator_Enum;
	    when One_Assign_Op =>
	      Assign_Op : Assign_Stmt.Assign_Operator_Enum;
	    when others => null;
	  end case;
	when Construct_Kind =>
	  Exitable_Construct : Control_Stmt.Exitable_Construct_Enum;
        when Optional_End_Token =>
          Label : Optional_Tree := Null_Optional_Tree;
          End_With_Values : Optional_Tree := Null_Optional_Tree;
          Check_Label : Boolean := False;
      end case;
    end record;

    use type Param_Decl.Param_Kind;

    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, ',', ';',
         ':', '.', '+',
         '-', '*', '/',
         '?', '(', ')',
         '[', ']', '<',
         '>', '|', '=',
         Prime, L_Assert, R_Assert,
         L_Set, R_Set, Compare,
         Eq, Neq, Geq,
         Leq, Lshift, Power,
         Colon_Equal, Swap, Move,
         Combine_Move, Dot_Dot, Open_Closed_Interval,
         Open_Interval, Closed_Open_Interval, Double_Colon,
         Refers_To, Gives, Implies,
         Parallel, Plus_Assign, Minus_Assign,
         Times_Assign, Divide_Assign, Power_Assign,
         Combine_Assign, And_Assign, Or_Assign,
         Xor_Assign, Lshift_Assign, Rshift_Assign,
         Newline, Outdent, Indent,
         Eol_Colon, Plus_Based_Op, Minus_Based_Op,
         Mul_Based_Op, Div_Based_Op, Exp_Based_Op,
         Or_Based_Op, And_Based_Op, Xor_Based_Op,
         Not_Based_Op, Lshift_Based_Op, Rshift_Based_Op,
         Combine_Based_Op, Char_Literal, Enum_Literal,
         Integer_Literal, Real_Literal, String_Literal,
         Identifier, Abs_Kw, Abstract_Kw,
         All_Kw, And_Kw, As_Kw,
         Begin_Kw, Block_Kw, Break_Kw,
         Case_Kw, Class_Kw, Concurrent_Kw,
         Const_Kw, Continue_Kw, Def_Kw,
         Defop_Kw, Each_Kw, Else_Kw,
         Elsif_Kw, End_Kw, Exit_Kw,
         Exports_Kw, Extends_Kw, For_Kw,
         Forward_Kw, Func_Kw, Global_Kw,
         If_Kw, Implements_Kw, Import_Kw,
         In_Kw, Interface_Kw, Is_Kw,
         Lambda_Kw, Locked_Kw, Loop_Kw,
         Mod_Kw, New_Kw, Not_Kw,
         Null_Kw, Of_Kw, Opt_Kw,
         Optional_Kw, Or_Kw, Private_Kw,
         Queued_Kw, Ref_Kw, Rem_Kw,
         Return_Kw, Reverse_Kw, Some_Kw,
         Switch_Kw, Then_Kw, Type_Kw,
         Until_Kw, Var_Kw, While_Kw,
         With_Kw, Xor_Kw );

    Syntax_Error : exception;

end Parython_Tokens;
