
pragma Style_Checks (Off);
package body Parython_Lex is

  --  Indicates whether outdenting should expect a perfect col-count match
  Col_Match_Expected : Boolean := True;

  Max_Depth : constant := 100;
  Indent_Stack : array(Positive range 1 .. Max_Depth) of Natural :=
    (others => 0);
  Bracketing_Token_Stack : array(Positive range 1 .. Max_Depth) of Token :=
    (others => Error);

  procedure ECHO_L(YYT : String := yytext) is
  --
  -- Local version of the  define string.
  -- 
  begin
     Echo_Token (YYT);
     --  We normally expect an exact match
     Col_Match_Expected := True;
  end ECHO_L;

  procedure Init is
  begin
     --  Reset indent level stack.
     Top := 0;
  end Init;

  function Create_Token(Text : String := yytext) return YYSType is
     Src_Pos : Source_Position := Cur_Source_Pos;
  begin
     --  Point to beginning of token
     Src_Pos.Col := Column_Number'Max (1, Src_Pos.Col - Text'Length + 1);
     --  TBD: "gcc" prefers column count rather than character count

     if Debug_Indent and then Expecting_Indent then
        Text_IO.Put (" [CT: indent off] ");
     end if;
     Expecting_Indent := False;

     --  Create a token for the parser.
     return (One_Token, Src_Pos, String_Lookup(Text));
  end Create_Token;
     
function YYLex return Token is
subtype short is integer range -32768..32767;
    yy_act : integer;
    yy_c : short;

-- returned upon end-of-file
YY_END_TOK : constant integer := 0;
YY_END_OF_BUFFER : constant := 161;
subtype yy_state_type is integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
TICK : constant := 1;
CHARLIT : constant := 2;
AFTER_IMPLEMENTS : constant := 3;
OUTDENTING : constant := 4;
RESCANNING : constant := 5;
yy_accept : constant array(0..560) of short :=
    (   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,  161,  159,  149,  158,  159,  159,  159,  115,
      116,  125,  126,  127,  128,  129,  130,  139,  139,  132,
      133,  119,  136,  120,  135,  137,  117,  118,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  121,
      134,  123,    6,  137,  137,  137,  137,  137,  137,  137,
      134,  114,  159,    6,  155,  160,  155,  155,  155,  155,
      155,  160,  160,  160,  160,  160,  160,  160,  160,  160,
      149,  158,    0,    0,    0,   83,    0,  146,    0,  147,

      138,   87,  103,  113,  101,  148,  102,   97,  113,   91,
      104,    0,    0,  139,    0,    0,    0,    0,    0,  131,
        0,  113,    0,   95,   88,    0,   86,   85,    0,  124,
       82,   96,   81,   84,    0,  137,  137,  137,  137,   20,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,  137,  137,   46,  137,   49,   51,  137,  137,
      137,  137,  137,  137,   60,  137,   64,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  122,
      106,  100,    6,    0,    0,    0,    0,    0,    0,  137,
      137,  137,  137,  137,  137,  137,   49,  137,  137,  100,

        0,    0,    6,    0,  154,    0,    0,    0,  153,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       99,  147,  138,  105,  148,   94,    0,  140,  144,    0,
      139,  143,    0,   93,  111,   89,   90,  107,   98,  112,
       16,   18,   19,  137,  137,  137,  137,  137,  137,   44,
      137,  137,  137,   36,  137,  137,  137,   41,  137,  137,
      137,  137,  137,  137,  137,   55,   57,   58,  137,   62,
      109,  137,  137,   67,   68,  137,  137,  137,  137,  137,
      137,  137,   77,  137,  137,   80,    4,    0,    0,    0,
        0,    0,   16,  137,  137,    6,    6,  137,  137,  137,

       57,  137,    3,  145,    0,    0,    0,    0,    0,    0,
        0,   12,   10,    0,    0,    0,    0,   56,    0,  141,
        0,    0,    0,  140,    0,    0,  144,    0,  139,    0,
      143,    0,    0,   92,  137,  108,  137,  137,  137,   24,
      137,  137,  137,  137,  137,   29,   32,   30,  137,  137,
       37,  137,  137,  137,   43,  137,  137,  137,  137,  137,
      137,   54,   59,  137,  137,  137,  137,  137,   71,  137,
       74,   75,  137,  137,   79,  110,    0,    1,    0,  137,
      137,  137,  137,  137,  137,  137,   74,    0,    0,  152,
        0,    0,    0,    0,    0,    0,   11,    0,    0,   73,

        0,    0,    0,  140,  137,   21,   22,   23,   25,  137,
       27,  137,   61,    0,  137,   34,   35,  137,  137,  137,
      137,  137,  137,  137,  137,  137,  137,  137,  137,  137,
      137,  137,   76,   78,    0,  137,    6,  137,    6,  137,
      137,  137,    5,    0,  150,    0,    8,    0,   15,    0,
        0,    0,    0,  141,  142,    0,    0,  140,    0,  137,
      137,  137,    0,   31,  137,  137,  137,   45,  137,   48,
      137,   52,   53,  137,  137,   66,   69,  137,   72,    0,
      137,  137,  137,    6,  137,    0,    0,    0,    0,   13,
        0,  141,    0,    0,  137,  137,  137,   33,   39,   40,

       42,  137,  137,  137,   65,   70,    0,  137,  137,   39,
      137,    0,    0,    0,   38,    0,    0,  142,   17,  137,
       28,    0,   39,  137,  137,   63,    0,    6,  137,    2,
        2,   39,  137,    0,    7,    0,    0,   38,    0,  142,
        0,  137,  137,   50,    2,    0,  137,    0,    6,    0,
        0,   14,   26,   47,    0,    6,    0,    9,  151,    0
    ) ;

yy_ec : constant array(ASCII.NUL..Character'Last) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    4,    4,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    5,    6,    7,    8,    1,    1,    1,    9,   10,
       11,   12,   13,   14,   15,   16,   17,   18,   19,   19,
       19,   19,   19,   19,   19,   19,   19,   20,   21,   22,
       23,   24,   25,    1,   26,   27,   26,   26,   28,   26,
       29,   29,   29,   29,   29,   29,   29,   29,   29,   29,
       29,   29,   29,   29,   29,   29,   29,   30,   29,   29,
       31,   32,   33,    1,   34,    1,   35,   36,   37,   38,

       39,   40,   41,   42,   43,   29,   44,   45,   46,   47,
       48,   49,   50,   51,   52,   53,   54,   55,   56,   57,
       58,   29,   59,   60,   61,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    ) ;

yy_meta : constant array(0..61) of short :=
    (   0,
        1,    2,    3,    2,    2,    1,    1,    4,    5,    1,
        1,    6,    1,    1,    1,    7,    1,    8,    8,    1,
        1,    1,    6,    1,    1,    9,    9,    9,   10,   10,
        1,    1,    1,   11,    9,    9,    9,    9,    9,    9,
       10,   10,   10,   10,   10,   10,   10,   10,   10,   10,
       10,   10,   10,   10,   10,   10,   10,   10,    1,    1,
        1
    ) ;

yy_base : constant array(0..578) of short :=
    (   0,
        0,   60,  924,  119,  923,  178,  922,  237,   65,   79,
        0,   32,  930,  934,   85,  934,  906,   73,  125,  934,
      934,   51,   94,  934,  119,   65,   92,  290,  157,  147,
      934,  172,  112,   60,  934,    0,  934,  934,  145,  102,
       71,  889,  154,   90,  882,  158,   38,  878,   92,  163,
      874,  870,  884,   48,  103,  875,  886,  126,  872,  895,
       88,  934,  346,  182,  174,  879,  200,  197,  154,  142,
      163,  934,  885,  246,  934,  934,    0,  870,  876,  872,
      853,  876,  126,  872,  173,  856,  131,  870,  866,  847,
      250,  934,  258,  891,  888,  934,  261,  934,  901,    0,

        0,  880,  934,  934,  934,    0,  934,  934,  886,  879,
      934,    0,  188,  273,  251,  264,    0,  269,  311,  934,
      328,  880,  882,  934,  934,  882,  874,  271,  873,  934,
      871,  934,  934,  934,  871,    0,  841,  847,  853,    0,
      849,  841,  849,  835,  851,  838,  844,  846,  253,  844,
      278,  830,  833,  831,    0,  829,  824,    0,  830,  262,
      837,  818,  820,  827,    0,  818,  847,  826,  829,  300,
      821,  823,  826,  815,  810,  811,  818,  807,  808,  934,
      934,  934,    0,    0,  801,  805,  817,  813,  794,  801,
      817,  804,  810,  811,  292,  799,  794,  790,  806,  362,

      835,  334,    0,  795,  934,  793,  785,  801,  934,  787,
      803,  790,  796,  797,  785,  786,  783,  778,  774,  790,
      934,    0,    0,  934,    0,  934,  326,  352,  304,  284,
      338,  794,  824,  804,  934,  934,  934,  934,  934,  934,
      772,    0,  801,  780,  785,  786,  781,  767,  322,  770,
      775,  776,  120,  772,  761,  765,  773,  755,  773,  773,
      211,  769,  771,  762,  756,    0,    0,    0,  759,  760,
      934,  747,  747,    0,    0,  746,  760,  759,  744,  749,
      756,  751,    0,  748,  750,  768,  934,  741,  374,  733,
      749,  379,  734,  734,  350,  737,  741,  735,  347,  743,

      405,  734,  934,  934,  729,  734,  409,  731,  724,  724,
      738,  726,  934,  725,  735,  723,  731,  934,  722,  272,
        0,  382,  388,  399,  406,  350,  410,  382,  412,  416,
      734,  733,  763,  934,  714,  934,  717,  719,  718,    0,
      709,  706,  706,  715,  708,    0,    0,  235,  716,  715,
        0,  703,  706,  717,    0,  716,  711,  698,  697,  709,
      707,    0,    0,  697,  709,  704,  691,  690,    0,  703,
        0,    0,  694,  699,    0,  934,  689,  934,  689,  684,
      682,  679,  683,  680,  679,  678,  437,  445,  689,  934,
      449,  676,  674,  671,  675,  672,  934,  671,  670,  934,

      442,  354,  418,  440,  685,    0,    0,    0,    0,  668,
        0,  671,    0,  674,  676,    0,    0,  662,  676,  662,
      667,  665,  657,  669,  673,  669,  659,  652,  666,  656,
      650,  659,    0,    0,  649,  664,    0,  647,    0,  644,
      643,  655,  934,  648,  934,  658,  934,  640,  934,  635,
      634,  646,  444,  446,  366,  389,  435,  448,  452,  633,
      615,  608,  621,    0,  602,  584,  583,    0,  574,    0,
      566,    0,    0,  555,  550,    0,    0,  549,    0,  534,
      548,  497,  495,    0,  511,  506,  507,  492,  490,  934,
      498,  454,  457,  466,  468,  469,  448,  934,  481,    0,

        0,  421,  419,  400,    0,    0,  380,  373,  381,  487,
      378,  326,  299,  305,  491,  302,  459,  476,    0,  278,
        0,  495,  934,  251,  246,    0,  500,    0,  226,  504,
      934,  514,  232,  208,  934,  189,  520,  934,  189,  480,
      494,  173,  172,    0,  521,  525,   48,  529,    0,   42,
       13,  934,    0,    0,  530,    0,  534,  934,  934,  934,
      550,  561,  572,  582,  585,  596,  607,  618,  622,  633,
      637,  639,  648,  656,  660,  671,  675,  681
    ) ;

yy_def : constant array(0..578) of short :=
    (   0,
      560,    1,    1,    1,    1,    1,    1,    1,  561,  561,
      562,  562,  560,  560,  560,  560,  560,  563,  564,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  565,  560,  560,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  560,
      560,  560,  560,  565,  565,  565,  565,  565,  565,  565,
      560,  560,  566,   63,  560,  560,  567,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  563,  560,  563,  568,

      569,  560,  560,  560,  560,  570,  560,  560,  560,  560,
      560,  571,  560,  560,  560,  560,  572,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  560,
      560,  560,   63,  573,  560,  560,  560,  560,  560,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  560,

      560,  566,   74,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  568,  569,  560,  570,  560,  574,  560,  560,  560,
      560,  575,  576,  560,  560,  560,  560,  560,  560,  560,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      560,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  560,  560,  560,  560,
      560,  560,  565,  565,  565,  565,  565,  565,  565,  565,

      565,  565,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      577,  574,  574,  560,  560,  560,  560,  560,  560,  560,
      575,  575,  576,  560,  565,  560,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  560,  560,  560,  560,  565,
      565,  565,  565,  565,  565,  565,  565,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,

      560,  578,  560,  560,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  560,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  560,  565,  565,  565,  565,  565,
      565,  565,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  578,  578,  560,  560,  565,
      565,  565,  560,  565,  565,  565,  565,  565,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,  565,  560,
      565,  565,  565,  565,  565,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  565,  565,  565,  560,  565,  565,

      565,  565,  565,  565,  565,  565,  560,  565,  565,  565,
      565,  560,  560,  560,  560,  560,  560,  560,  565,  565,
      565,  560,  560,  565,  565,  565,  560,  565,  565,  560,
      560,  560,  565,  560,  560,  560,  560,  560,  560,  560,
      560,  565,  565,  565,  560,  560,  565,  560,  565,  560,
      560,  560,  565,  565,  560,  565,  560,  560,  560,    0,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560
    ) ;

yy_nxt : constant array(0..995) of short :=
    (   0,
       14,   15,   16,   15,   15,   17,   18,   19,   14,   20,
       21,   22,   23,   24,   25,   26,   27,   28,   29,   30,
       31,   32,   33,   34,   35,   36,   36,   36,   36,   36,
       37,   14,   38,   14,   39,   40,   41,   42,   43,   44,
       45,   36,   46,   36,   47,   48,   49,   50,   51,   52,
       53,   54,   55,   56,   57,   58,   59,   36,   60,   61,
       62,   63,  102,   63,   63,  558,   82,   76,   83,   84,
       85,   86,  159,  103,   87,   98,   77,  109,   88,   98,
      110,   76,  134,  135,   89,  160,   91,   92,   91,   91,
       77,   90,   93,  557,   64,  171,   65,   66,   67,   94,

      556,   95,   68,  172,   99,  144,   69,   78,  100,  104,
      181,   79,   70,  104,  111,  145,  105,   80,  146,   71,
       63,   78,   63,   63,   81,   79,  100,   72,  100,  100,
      162,   80,  100,  106,  131,  132,  133,  152,   81,  163,
      141,  107,  108,  153,  173,  164,  142,  182,  119,  120,
      119,  119,  143,   64,  121,   65,   66,   67,  348,  122,
      174,   68,  349,  123,  112,   69,  124,  177,  178,  125,
      211,   70,  113,  212,  114,  114,  217,  218,   71,   63,
      137,   63,   63,  199,  116,  181,   73,  126,  148,  138,
      118,  139,  198,  127,  128,  116,  140,  155,  149,  174,

      150,  163,  165,  156,  157,  228,  228,  164,  144,  158,
      151,  166,   64,  167,   65,   66,   67,  190,  191,  214,
       68,  192,  200,  554,   69,  553,  138,  552,  139,  215,
       70,  129,  130,  140,  148,  551,  155,   71,   74,  414,
       74,   74,  196,  197,  149,   73,  194,  203,  158,  203,
      203,   91,   92,   91,   91,  357,  195,   93,  358,  100,
      550,  100,  100,   98,   94,  100,   95,   98,  229,  229,
      549,   64,  547,   65,   66,   67,  230,  415,  230,   68,
      112,  231,  231,   69,  544,  204,  114,  114,  113,   70,
      114,  114,   99,  236,  237,  252,   71,  112,  264,  401,

      116,  231,  231,  543,  253,  113,  118,  114,  114,  265,
      401,  116,  119,  120,  119,  119,  115,  116,  121,  117,
      255,  327,  327,  118,  542,  115,  256,  123,  116,  233,
      257,  233,  233,  320,  255,  233,  560,  328,  539,  274,
      298,  321,  201,  536,  257,  275,  117,  183,   92,  183,
      183,  535,  276,   93,  277,  329,  329,  184,  342,  323,
       94,  455,   95,  303,  303,  303,  303,  324,  324,  324,
      324,  330,  534,  343,  344,  378,  378,  378,  378,  325,
      303,  303,  303,  303,  185,  326,  382,  457,  186,  320,
      325,  357,  187,  494,  385,  560,  455,  321,  188,  327,

      327,  343,  344,  560,  494,  189,  303,  303,  303,  303,
      390,  390,  390,  390,  533,  323,  324,  324,  403,  529,
      403,  560,  457,  404,  404,  528,  325,  327,  327,  329,
      329,  527,  326,  329,  329,  404,  404,  325,  303,  303,
      303,  303,  560,  328,  526,  330,  443,  443,  443,  443,
      445,  445,  445,  445,  453,  525,  453,  458,  458,  454,
      454,  454,  454,  492,  492,  458,  458,  524,  560,  458,
      458,  492,  492,  459,  492,  492,  518,  518,  517,  493,
      517,  459,  522,  518,  518,  522,  521,  493,  530,  531,
      531,  530,  537,  540,  540,  537,  522,  540,  540,  522,

      523,  545,  531,  531,  545,  548,  532,  520,  548,  541,
      538,  540,  540,  541,  523,  531,  531,  531,  531,  546,
      519,  537,  555,  532,  537,  555,  531,  531,  531,  531,
      548,  555,  516,  548,  555,  559,  559,  559,  559,  538,
      546,  515,  514,  513,  512,  511,  510,  509,  532,  546,
       75,   75,   75,   75,   75,   75,   75,   75,   75,   75,
       75,   76,   76,   76,   76,   76,   76,   76,   76,   76,
       76,   76,   97,   97,   97,   97,   97,   97,   97,   97,
       97,   97,   97,  101,  508,  101,  507,  506,  505,  504,
      101,  101,  136,  136,  136,  136,  201,  201,  201,  201,

      503,  201,  201,  201,  201,  201,  201,  205,  205,  205,
      205,  205,  502,  205,  205,  205,  205,  205,  222,  222,
      501,  222,  222,  222,  222,  222,  222,  222,  222,  223,
      223,  223,  223,  225,  225,  500,  225,  225,  225,  225,
      225,  225,  225,  225,  227,  227,  232,  232,  287,  287,
      287,  287,  287,  499,  287,  287,  287,  287,  287,  322,
      498,  497,  322,  322,  322,  496,  322,  331,  331,  495,
      331,  333,  333,  333,  333,  333,  333,  333,  333,  333,
      333,  333,  402,  402,  456,  491,  490,  489,  456,  456,
      488,  456,  487,  486,  485,  484,  483,  482,  481,  480,

      479,  478,  477,  476,  475,  474,  473,  472,  471,  470,
      469,  468,  467,  466,  465,  464,  463,  462,  461,  460,
      452,  451,  450,  449,  448,  447,  446,  444,  442,  441,
      440,  439,  438,  437,  436,  292,  435,  434,  433,  432,
      431,  430,  429,  428,  427,  426,  425,  424,  423,  422,
      421,  420,  419,  418,  417,  416,  413,  412,  411,  410,
      409,  408,  407,  406,  405,  120,  560,  332,  400,  399,
      398,  397,  396,  395,  394,  393,  392,  391,  389,  388,
      387,  386,  384,  350,  383,  381,  380,  379,  292,  377,
      376,  375,  374,  373,  372,  371,  370,  369,  368,  367,

      366,  365,  364,  363,  362,  361,  360,  359,  356,  355,
      354,  353,  352,  351,  350,  347,  346,  345,  341,  340,
      339,  338,  337,  336,  335,  334,  120,  332,  319,  318,
      317,  316,  315,  314,  313,  312,  311,  310,  309,  308,
      307,  306,  305,  304,  302,  301,  300,  299,  297,  296,
      295,  294,  293,  292,  291,  290,  289,  288,  286,  285,
      284,  283,  282,  281,  280,  279,  278,  273,  272,  271,
      270,  269,  268,  267,  266,  263,  262,  261,  260,  259,
      258,  254,  251,  250,  249,  248,  247,  246,  245,  244,
      243,  242,  241,  240,  239,  238,  235,  234,  233,  104,

      226,  104,  224,  560,  100,  106,  221,  220,  219,  216,
      213,  210,  209,  208,  207,  206,  202,  193,  180,  179,
      176,  175,  170,  169,  168,  161,  154,  147,   96,  560,
       73,   73,   72,   13,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560

    ) ;

yy_chk : constant array(0..995) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    2,   22,    2,    2,  551,   12,    9,   12,   12,
       12,   12,   47,   22,   12,   18,    9,   26,   12,   18,
       26,   10,   34,   34,   12,   47,   15,   15,   15,   15,
       10,   12,   15,  550,    2,   54,    2,    2,    2,   15,

      547,   15,    2,   54,   18,   41,    2,    9,   27,   23,
       61,    9,    2,   23,   27,   41,   23,    9,   41,    2,
        4,   10,    4,    4,    9,   10,   19,    4,   19,   19,
       49,   10,   19,   25,   33,   33,   33,   44,   10,   49,
       40,   25,   25,   44,   55,   49,   40,   61,   30,   30,
       30,   30,   40,    4,   30,    4,    4,    4,  253,   30,
       55,    4,  253,   30,   29,    4,   30,   58,   58,   30,
       83,    4,   29,   83,   29,   29,   87,   87,    4,    6,
       39,    6,    6,   70,   29,   71,    6,   32,   43,   39,
       29,   39,   69,   32,   32,   29,   39,   46,   43,   70,

       43,   69,   50,   46,   46,  113,  113,   69,   65,   46,
       43,   50,    6,   50,    6,    6,    6,   64,   65,   85,
        6,   65,   71,  543,    6,  542,   64,  539,   64,   85,
        6,   32,   32,   64,   67,  536,   68,    6,    8,  348,
        8,    8,   68,   68,   67,    8,   67,   74,   68,   74,
       74,   91,   91,   91,   91,  261,   67,   91,  261,   93,
      534,   93,   93,   97,   91,   93,   91,   97,  115,  115,
      533,    8,  529,    8,    8,    8,  116,  348,  116,    8,
      114,  116,  116,    8,  525,   74,  118,  118,  114,    8,
      114,  114,   97,  128,  128,  149,    8,   28,  160,  320,

      114,  230,  230,  524,  149,   28,  114,   28,   28,  160,
      320,  114,  119,  119,  119,  119,   28,   28,  119,   28,
      151,  229,  229,   28,  520,   28,  151,  119,   28,  121,
      151,  121,  121,  227,  195,  121,  202,  229,  516,  170,
      195,  227,  202,  514,  195,  170,   28,   63,   63,   63,
       63,  513,  170,   63,  170,  231,  231,   63,  249,  227,
       63,  402,   63,  200,  200,  200,  200,  326,  326,  228,
      228,  231,  512,  249,  249,  289,  289,  289,  289,  228,
      292,  292,  292,  292,   63,  228,  295,  402,   63,  322,
      228,  299,   63,  455,  299,  323,  456,  322,   63,  328,

      328,  295,  295,  323,  455,   63,  301,  301,  301,  301,
      307,  307,  307,  307,  511,  322,  324,  324,  325,  509,
      325,  323,  456,  325,  325,  508,  324,  327,  327,  329,
      329,  507,  324,  330,  330,  403,  403,  324,  387,  387,
      387,  387,  457,  327,  504,  329,  388,  388,  388,  388,
      391,  391,  391,  391,  401,  503,  401,  404,  404,  401,
      401,  453,  453,  454,  454,  458,  458,  502,  457,  459,
      459,  492,  492,  404,  493,  493,  517,  517,  494,  454,
      494,  458,  499,  494,  494,  499,  497,  492,  510,  510,
      510,  510,  515,  518,  518,  515,  522,  540,  540,  522,

      499,  527,  527,  527,  527,  530,  510,  496,  530,  518,
      515,  541,  541,  540,  522,  532,  532,  532,  532,  527,
      495,  537,  545,  530,  537,  545,  546,  546,  546,  546,
      548,  555,  491,  548,  555,  557,  557,  557,  557,  537,
      545,  489,  488,  487,  486,  485,  483,  482,  548,  555,
      561,  561,  561,  561,  561,  561,  561,  561,  561,  561,
      561,  562,  562,  562,  562,  562,  562,  562,  562,  562,
      562,  562,  563,  563,  563,  563,  563,  563,  563,  563,
      563,  563,  563,  564,  481,  564,  480,  478,  475,  474,
      564,  564,  565,  565,  565,  565,  566,  566,  566,  566,

      471,  566,  566,  566,  566,  566,  566,  567,  567,  567,
      567,  567,  469,  567,  567,  567,  567,  567,  568,  568,
      467,  568,  568,  568,  568,  568,  568,  568,  568,  569,
      569,  569,  569,  570,  570,  466,  570,  570,  570,  570,
      570,  570,  570,  570,  571,  571,  572,  572,  573,  573,
      573,  573,  573,  465,  573,  573,  573,  573,  573,  574,
      463,  462,  574,  574,  574,  461,  574,  575,  575,  460,
      575,  576,  576,  576,  576,  576,  576,  576,  576,  576,
      576,  576,  577,  577,  578,  452,  451,  450,  578,  578,
      448,  578,  446,  444,  442,  441,  440,  438,  436,  435,

      432,  431,  430,  429,  428,  427,  426,  425,  424,  423,
      422,  421,  420,  419,  418,  415,  414,  412,  410,  405,
      399,  398,  396,  395,  394,  393,  392,  389,  386,  385,
      384,  383,  382,  381,  380,  379,  377,  374,  373,  370,
      368,  367,  366,  365,  364,  361,  360,  359,  358,  357,
      356,  354,  353,  352,  350,  349,  345,  344,  343,  342,
      341,  339,  338,  337,  335,  333,  332,  331,  319,  317,
      316,  315,  314,  312,  311,  310,  309,  308,  306,  305,
      302,  300,  298,  297,  296,  294,  293,  291,  290,  288,
      286,  285,  284,  282,  281,  280,  279,  278,  277,  276,

      273,  272,  270,  269,  265,  264,  263,  262,  260,  259,
      258,  257,  256,  255,  254,  252,  251,  250,  248,  247,
      246,  245,  244,  243,  241,  234,  233,  232,  220,  219,
      218,  217,  216,  215,  214,  213,  212,  211,  210,  208,
      207,  206,  204,  201,  199,  198,  197,  196,  194,  193,
      192,  191,  190,  189,  188,  187,  186,  185,  179,  178,
      177,  176,  175,  174,  173,  172,  171,  169,  168,  167,
      166,  164,  163,  162,  161,  159,  157,  156,  154,  153,
      152,  150,  148,  147,  146,  145,  144,  143,  142,  141,
      139,  138,  137,  135,  131,  129,  127,  126,  123,  122,

      110,  109,  102,   99,   95,   94,   90,   89,   88,   86,
       84,   82,   81,   80,   79,   78,   73,   66,   60,   59,
       57,   56,   53,   52,   51,   48,   45,   42,   17,   13,
        7,    5,    3,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560,  560,  560,  560,  560,  560,
      560,  560,  560,  560,  560

    ) ;


-- copy whatever the last rule matched to the standard output

procedure ECHO is
begin
   if (text_io.is_open(user_output_file)) then
     text_io.put( user_output_file, yytext );
   else
     text_io.put( yytext );
   end if;
end ECHO;

-- enter a start condition.
-- Using procedure requires a () after the ENTER, but makes everything
-- much neater.

procedure ENTER( state : integer ) is
begin
     yy_start := 1 + 2 * state;
end ENTER;

-- action number for EOF rule of a given start state
function YY_STATE_EOF(state : integer) return integer is
begin
     return YY_END_OF_BUFFER + state + 1;
end YY_STATE_EOF;

-- return all but the first 'n' matched characters back to the input stream
procedure yyless(n : integer) is
begin
        yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
        yy_cp := yy_bp + n;
        yy_c_buf_p := yy_cp;
        YY_DO_BEFORE_ACTION; -- set up yytext again
end yyless;

-- redefine this if you have something you want each time.
procedure YY_USER_ACTION is
begin
        null;
end;

-- yy_get_previous_state - get the state just before the EOB char was reached

function yy_get_previous_state return yy_state_type is
    yy_current_state : yy_state_type;
    yy_c : short;
    yy_bp : integer := yytext_ptr;
begin
    yy_current_state := yy_start;
    if ( yy_ch_buf(yy_bp-1) = ASCII.LF ) then
	yy_current_state := yy_current_state + 1;
    end if;

    for yy_cp in yytext_ptr..yy_c_buf_p - 1 loop
	yy_c := yy_ec(yy_ch_buf(yy_cp));
	if ( yy_accept(yy_current_state) /= 0 ) then
	    yy_last_accepting_state := yy_current_state;
	    yy_last_accepting_cpos := yy_cp;
	end if;
	while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
	    yy_current_state := yy_def(yy_current_state);
	    if ( yy_current_state >= 561 ) then
		yy_c := yy_meta(yy_c);
	    end if;
	end loop;
	yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
    end loop;

    return yy_current_state;
end yy_get_previous_state;

procedure yyrestart( input_file : file_type ) is
begin
   open_input(text_io.name(input_file));
end yyrestart;

begin -- of YYLex
<<new_file>>
        -- this is where we enter upon encountering an end-of-file and
        -- yywrap() indicating that we should continue processing

    if ( yy_init ) then
        if ( yy_start = 0 ) then
            yy_start := 1;      -- first start state
        end if;

        -- we put in the '\n' and start reading from [1] so that an
        -- initial match-at-newline will be true.

        yy_ch_buf(0) := ASCII.LF;
        yy_n_chars := 1;

        -- we always need two end-of-buffer characters.  The first causes
        -- a transition to the end-of-buffer state.  The second causes
        -- a jam in that state.

        yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
        yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

        yy_eof_has_been_seen := false;

        yytext_ptr := 1;
        yy_c_buf_p := yytext_ptr;
        yy_hold_char := yy_ch_buf(yy_c_buf_p);
        yy_init := false;
    end if; -- yy_init

    loop                -- loops until end-of-file is reached


        yy_cp := yy_c_buf_p;

        -- support of yytext
        yy_ch_buf(yy_cp) := yy_hold_char;

        -- yy_bp points to the position in yy_ch_buf of the start of the
        -- current run.
	yy_bp := yy_cp;
	yy_current_state := yy_start;
	if ( yy_ch_buf(yy_bp-1) = ASCII.LF ) then
	    yy_current_state := yy_current_state + 1;
	end if;
	loop
		yy_c := yy_ec(yy_ch_buf(yy_cp));
		if ( yy_accept(yy_current_state) /= 0 ) then
		    yy_last_accepting_state := yy_current_state;
		    yy_last_accepting_cpos := yy_cp;
		end if;
		while ( yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state ) loop
		    yy_current_state := yy_def(yy_current_state);
		    if ( yy_current_state >= 561 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 560 ) then
    exit;
end if;
	end loop;
	yy_cp := yy_last_accepting_cpos;
	yy_current_state := yy_last_accepting_state;

<<next_action>>
	    yy_act := yy_accept(yy_current_state);
            YY_DO_BEFORE_ACTION;
            YY_USER_ACTION;

        if aflex_debug then  -- output acceptance info. for (-d) debug mode
            text_io.put( Standard_Error, "--accepting rule #" );
            text_io.put( Standard_Error, INTEGER'IMAGE(yy_act) );
            text_io.put_line( Standard_Error, "(""" & yytext & """)");
        end if;


<<do_action>>   -- this label is used only to access EOF actions
            case yy_act is
		when 0 => -- must backtrack
		-- undo the effects of YY_DO_BEFORE_ACTION
		yy_ch_buf(yy_cp) := yy_hold_char;
		yy_cp := yy_last_accepting_cpos;
		yy_current_state := yy_last_accepting_state;
		goto next_action;



-- ^[ \r\t\f]+/"queued"[ \r\t\f\n] {
--         -- ignore "queued" at start of line as far as indenting
--         ECHO_L; 
--         if Debug_Indent and then Expecting_Indent then
--            Text_IO.Put(" [queued: indent off] ");
--         end if;
--         Expecting_Indent := False;  -- "then" will turn it back on
-- }
when 1 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 3;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 50 "parython_lex.l"

        -- ignore "is" at start of line as far as indenting
        ECHO_L; 


when 2 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 55 "parython_lex.l"

        -- this keyword when used at the beginning of a line
        -- is expected to be "outdented" relative to the preceding and
        -- following construct.  We want to treat it as though it
        -- is the same indent as the prior construct that is just slightly
        -- more indented than this keyword.
        -- However, if we are expecting an *indent* then we don't look
        -- at the stack, and just return an INDENT, while preserving
        -- the "expecting indent" flag.
        -- If not expecting an indent, then we generate zero or more
        -- OUTDENTs and one NEWLINE if it is outdented relative to
        -- the top of the indent stack.  If not outdented relative to
        -- the top of the indent stack, we don't produce anything extra.

    declare
        YYT : constant String := yytext;
        Export_Start : Positive := YYT'First;
    begin
        pragma Assert (Col_Count = 0);

        --  Start of line, determine indent
        while Export_Start < YYT'Last and then YYT(Export_Start) /= 'e' loop
           Export_Start := Export_Start + 1;
        end loop;

        ECHO_L(YYT(YYT'First .. Export_Start - 1));

        --  Put "exports..." back
        for I in reverse Export_Start .. YYT'Last loop
           unput(YYT(I));
        end loop;

        if Col_Count = 0 then
           ENTER(RESCANNING);
        end if;

        --  At this point, Col_Count is indent level

        if Expecting_Indent then
           yylval := Create_Token(Text => "");
           if Debug_Indent then
              Text_IO.Put_Line(" [returning INDENT] ");
              Text_IO.Flush;
           end if;
           --  Preserve indent expectation
           Expecting_Indent := True;
           return INDENT;
        elsif Top = 0 then
           --  No indents on stack yet
           --  This shouldn't happen.
           yyerror("Source file cannot start with this token");
        elsif Col_Count >= Indent_Stack(Top) then
           --  Indenting relative to remembered stack
           --  Do nothing
           null;
        elsif Top = 1 then
           --  This is unexpected; should be at least two
           --  Do nothing
           null;
        elsif Col_Count >= Indent_Stack(Top-1) then
           --  See where we stand relative to the next indent level.
           --  We are not more outdented than next level, so return NEWLINE
           yylval := Create_Token(Text => "");
           if Debug_Indent then
              Text_IO.Put_Line(" [returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           return NEWLINE;
        else
           --  Outdenting relative to remembered indent;
           --  we will set Col_Match_Expected to False so we
           --  won't complain about a col-count mismatch.
           pragma Assert (Col_Count < Indent_Stack (Top-1));

           Col_Match_Expected := False;

           if Debug_Indent then
              Text_IO.Put_Line(" [entering OUTDENTING; returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           ENTER(OUTDENTING);
           yylval := Create_Token(Text => "");
           return NEWLINE;
        end if;
        if Debug_Indent and then Expecting_Indent then
           Text_IO.Put(" [NL: indent off] ");
        end if;
        Expecting_Indent := False;
    end;


when 3 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 146 "parython_lex.l"

        -- ignore "new", "then", "||" at start of line as far as indenting
        -- but return a NEWLINE if not expecting an INDENT
    declare
        YYT : constant String := yytext;
    begin

        pragma Assert (Col_Count = 0);

        for I in reverse YYT'Range loop
           if YYT(I) <= ' ' then
              --  Echo the spaces at the front of the line
              ECHO_L(YYT(YYT'First .. I));
              exit;
           end if;
           --  Unput the non-space characters
           unput(YYT(I));
        end loop;

        if Col_Count = 0 then
           --  Next time, return the token
           ENTER(RESCANNING);
        end if;

        if not Expecting_Indent then
           yylval := Create_Token(Text => "");
           return NEWLINE;
        end if;
    end;


when 4 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 2;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 177 "parython_lex.l"

        -- Remember Expecting_Indent state, and return INDENT
        -- if expecting indent, but don't push new indent level on stack;
        -- return NEWLINE if not expecting an indent.
        Expecting_Indent_At_Label := Expecting_Indent;
        ECHO_L; 
        if Expecting_Indent then
            yylval := Create_Token(Text => "");
            if Debug_Indent then
               Text_IO.Put_Line(" [label: returning INDENT] "); Text_IO.Flush;
            end if;
            Expecting_Indent := True;
            return INDENT;
        else
            yylval := Create_Token(Text => "");
            return NEWLINE;
        end if;


when 5 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
yy_cp := yy_cp - 4;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 196 "parython_lex.l"

        -- ignore "for" after "implements" at start of line as far as indenting
        ECHO_L; 
        ENTER(CHARLIT);


when 6 => 
--# line 202 "parython_lex.l"

    declare
        -- ignore spaces,Carriage returns,tabs,form feeds
        -- 
        -- determine indent (expanding tabs appropriately)
        -- compare to top of indent "stack"
        -- if less, emit OUTDENTs while popping from stack until find match
        --   (except ignore such a line if it starts with 
        --     "then"/"new"/"implements"/"exports"/"||"/"*XXX*")
        --   * complain if new level does not match one already on stack
        -- if same, emit NEWLINE; leave stack as is.
        -- if indent is more than top of stack:
        --   * if prior line ends with ":", push indent on stack and emit INDENT
        --   * if prior line does *not* end with ":" do not push indent on stack

        YYT : constant String := yytext;
    begin

        pragma Assert (Col_Count = 0);

        if YYT(YYT'First) in 'a'..'z' then
           --  Indent is zero; put back characters
           for I in reverse YYT'Range loop
              unput(YYT(I));
           end loop;
           --  Next time, return the token
           ENTER(RESCANNING);
        else
           --  Start of line, determine indent
           ECHO_L; 
        end if;

        --  At this point, Col_Count is indent level

        if Top = 0 then
           --  No indents on stack yet
           Top := Top + 1;
           Indent_Stack(Top) := Col_Count;
           Bracketing_Token_Stack(Top) := Error;
           if Debug_Indent then
              Text_IO.Put_Line(" [pushing" & Integer'Image(Col_Count) &
                " on Indent_Stack] ");
              Text_IO.Flush;
           end if;
        elsif Col_Count > Indent_Stack(Top) then
           --  Indenting relative to remembered stack
           if Expecting_Indent then
              --  Indenting is only significant if
              --  preceded by ':'/is/of/then/else/loop
              Top := Top + 1;
              Indent_Stack(Top) := Col_Count;
              Bracketing_Token_Stack(Top) := Bracketing_Token;
              if Debug_Indent then
                 Text_IO.Put_Line(" [pushing" & Integer'Image(Col_Count) &
                   " on Indent_Stack] "); Text_IO.Flush;
              end if;
              yylval := Create_Token(Text => "");
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning INDENT] "); Text_IO.Flush;
              end if;
              return INDENT;
           end if;
        elsif Col_Count = Indent_Stack(Top) then
           --  Same indent, this is a "significant" newline
           yylval := Create_Token(Text => "");
           if Debug_Indent then
              Text_IO.Put_Line(" [returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           return NEWLINE;
        else
           --  Outdenting relative to remembered indent;
           --  we will pop indent-stack until we get a match
           pragma Assert (Col_Count < Indent_Stack (Top));

           --  tbd: convert last OUTDENT into a NEWLINE: Top := Top - 1;
           if Debug_Indent then
              Text_IO.Put_Line(" [entering OUTDENTING; returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           ENTER(OUTDENTING);
           yylval := Create_Token(Text => "");
           return NEWLINE;
        end if;
        if Debug_Indent and then Expecting_Indent then
           Text_IO.Put(" [NL: indent off] ");
        end if;
        Expecting_Indent := False;
    end;


when 7 => 
--# line 293 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABSTRACT_kw);


when 8 => 
--# line 299 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CLASS_kw);


when 9 => 
--# line 305 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONCURRENT_kw);


when 10 => 
--# line 311 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (END_kw);


when 11 => 
--# line 317 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);


when 12 => 
--# line 323 "parython_lex.l"
  --  "def" is used in Python, equiv to "func"
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DEF_kw);


when 13 => 
--# line 329 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IMPORT_kw);


when 14 => 
--# line 335 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (INTERFACE_kw);


when 15 => 
--# line 341 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DEFOP_kw);


  -- Parython reserved words
when 16 => 
--# line 348 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABS_kw);


when 17 => 
--# line 353 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABSTRACT_kw);


when 18 => 
--# line 358 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ALL_kw);


when 19 => 
--# line 363 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (AND_kw);


when 20 => 
--# line 368 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (AS_kw);


when 21 => 
--# line 373 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (BEGIN_kw);


when 22 => 
--# line 378 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := BLOCK_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (BLOCK_kw);


when 23 => 
--# line 388 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (BREAK_kw);


when 24 => 
--# line 393 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CASE_kw);


when 25 => 
--# line 398 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CLASS_kw);


when 26 => 
--# line 403 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONCURRENT_kw);


when 27 => 
--# line 408 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONST_kw);


when 28 => 
--# line 413 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONTINUE_kw);


when 29 => 
--# line 418 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EACH_kw);


when 30 => 
--# line 423 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := ELSE_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (ELSE_kw);


when 31 => 
--# line 433 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
	yyerror("Use ""elsif"" rather than ""elseif""");
	return (ELSIF_kw);


when 32 => 
--# line 439 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
        --  Allow "elif" as an alias for "elsif" for Python-like syntax
	return (ELSIF_kw);


when 33 => 
--# line 445 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
	yyerror("Use ""elsif"" rather than ""else if""");
	return (ELSIF_kw);


when 34 => 
--# line 451 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ELSIF_kw);


when 35 => 
--# line 456 "parython_lex.l"

        unput('f'); unput('i');
	ECHO_L("end"); ENTER(CHARLIT); 
        yylval := Create_Token("end");
	yyerror("Use ""end if"" rather than ""endif""");
	return (END_kw);


when 36 => 
--# line 463 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (END_kw);


when 37 => 
--# line 468 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXIT_kw);


when 38 => 
--# line 473 "parython_lex.l"

        declare
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (EXPORTS_kw);
        end;


when 39 => 
--# line 486 "parython_lex.l"

        declare
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (EXPORTS_kw);
        end;


when 40 => 
--# line 499 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXTENDS_kw);


when 41 => 
--# line 504 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FOR_kw);


when 42 => 
--# line 509 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FORWARD_kw);


when 43 => 
--# line 514 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);


when 44 => 
--# line 519 "parython_lex.l"
  --  "def" is used in Python, equiv to "func"
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DEF_kw);


when 45 => 
--# line 524 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (GLOBAL_kw);


when 46 => 
--# line 529 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IF_kw);


when 47 => 
--# line 534 "parython_lex.l"

	ECHO_L; ENTER(AFTER_IMPLEMENTS); 
        --  TBD: We might want to return a NEWLINE first.
	yylval := Create_Token;
	return (IMPLEMENTS_kw);


when 48 => 
--# line 540 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IMPORT_kw);


when 49 => 
--# line 545 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IN_kw);


when 50 => 
--# line 550 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (INTERFACE_kw);


when 51 => 
--# line 555 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := IS_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (IS_kw);


when 52 => 
--# line 565 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LAMBDA_kw);


when 53 => 
--# line 570 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LOCKED_kw);


when 54 => 
--# line 575 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := LOOP_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (LOOP_kw);


when 55 => 
--# line 585 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (MOD_kw);


when 56 => 
--# line 590 "parython_lex.l"

        declare
           --  Preserve "Expecting_Indent" across "new"
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (NEW_kw);
        end;


when 57 => 
--# line 604 "parython_lex.l"

        declare
           --  Preserve "Expecting_Indent" across "new"
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (NEW_kw);
        end;


when 58 => 
--# line 618 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (NOT_kw);


when 59 => 
--# line 623 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (NULL_kw);


when 60 => 
--# line 628 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := OF_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (OF_kw);


when 61 => 
--# line 638 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DEFOP_kw);


when 62 => 
--# line 643 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OPT_kw);


when 63 => 
--# line 648 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OPTIONAL_kw);


when 64 => 
--# line 653 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OR_kw);


when 65 => 
--# line 658 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PRIVATE_kw);


when 66 => 
--# line 663 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (QUEUED_kw);


when 67 => 
--# line 668 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REF_kw);


when 68 => 
--# line 673 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REM_kw);


when 69 => 
--# line 678 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (RETURN_kw);


when 70 => 
--# line 683 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REVERSE_kw);


when 71 => 
--# line 688 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SOME_kw);


when 72 => 
--# line 693 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SWITCH_kw);


when 73 => 
--# line 698 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := THEN_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (THEN_kw);


when 74 => 
--# line 708 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := THEN_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (THEN_kw);


when 75 => 
--# line 718 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (TYPE_kw);


when 76 => 
--# line 723 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (UNTIL_kw);


when 77 => 
--# line 728 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (VAR_kw);


when 78 => 
--# line 733 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WHILE_kw);


when 79 => 
--# line 738 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WITH_kw);


when 80 => 
--# line 743 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (XOR_kw);


  -- Match all the compound Parython delimiters. 
when 81 => 
--# line 750 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMPARE);


when 82 => 
--# line 755 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(EQ);


when 83 => 
--# line 760 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(NEQ);


when 84 => 
--# line 765 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GEQ);


when 85 => 
--# line 770 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LEQ);


when 86 => 
--# line 775 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT);


when 87 => 
--# line 780 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER);


when 88 => 
--# line 785 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COLON_EQUAL);


when 89 => 
--# line 790 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MOVE);


when 90 => 
--# line 795 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(SWAP);


when 91 => 
--# line 800 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOT_DOT);


when 92 => 
--# line 805 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_INTERVAL);


when 93 => 
--# line 810 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_CLOSED_INTERVAL);


when 94 => 
--# line 815 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(CLOSED_OPEN_INTERVAL);


when 95 => 
--# line 820 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOUBLE_COLON);


when 96 => 
--# line 825 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(REFERS_TO);


when 97 => 
--# line 830 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GIVES);


when 98 => 
--# line 835 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(IMPLIES);


when 99 => 
--# line 840 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);


when 100 => 
--# line 845 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);


when 101 => 
--# line 850 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_ASSIGN);


when 102 => 
--# line 855 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MINUS_ASSIGN);


when 103 => 
--# line 860 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(TIMES_ASSIGN);


when 104 => 
--# line 865 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DIVIDE_ASSIGN);


when 105 => 
--# line 870 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER_ASSIGN);


when 106 => 
--# line 875 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_ASSIGN);


when 107 => 
--# line 880 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_MOVE);


when 108 => 
--# line 885 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(AND_ASSIGN);


when 109 => 
--# line 890 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OR_ASSIGN);


when 110 => 
--# line 895 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(XOR_ASSIGN);


when 111 => 
--# line 900 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT_ASSIGN);


when 112 => 
--# line 905 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(RSHIFT_ASSIGN);


when 113 => 
--# line 910 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_BASED_OP);


  -- Match all the Parython single-character delimiters.
when 114 => 
--# line 917 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(PRIME);


when 115 => 
--# line 922 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('(');


when 116 => 
--# line 927 "parython_lex.l"

	ECHO_L; ENTER(TICK); 
	yylval := Create_Token;
	return(')');


when 117 => 
--# line 932 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('[');


when 118 => 
--# line 937 "parython_lex.l"

	ECHO_L; ENTER(TICK); 
	yylval := Create_Token;
	return(']');


when 119 => 
--# line 942 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('<');


when 120 => 
--# line 947 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('>');


when 121 => 
--# line 952 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_SET);  --  or L_SET in PARython mode


when 122 => 
--# line 957 "parython_lex.l"
  --  Allow > ... < so PARython can use  ...  for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_ASSERT);


when 123 => 
--# line 962 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_SET);  --  or R_SET in PARython mode


when 124 => 
--# line 967 "parython_lex.l"
  --  Allow > ... < so PARython can use  ...  for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_ASSERT);


when 125 => 
--# line 972 "parython_lex.l"

        declare
           --  Remember Expecting_Indent
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           if Expecting_Indent_At_Label then
              if not Old_Indent then
                 --  Seen '*' twice; set Expecting_Indent back on
                 Expecting_Indent_At_Label := False;
                 Expecting_Indent := True;
                 if Debug_Indent then
                    Text_IO.Put(" [indent on] ");
                 end if;
              end if;
           end if;
	   return ('*');
        end;


when 126 => 
--# line 992 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('+');


when 127 => 
--# line 997 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(',');


when 128 => 
--# line 1002 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('-');


when 129 => 
--# line 1007 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('.');


when 130 => 
--# line 1012 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('/');


when 131 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1017 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
        Bracketing_Token := ':';
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return(EOL_COLON);


when 132 => 
--# line 1027 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(':');


when 133 => 
--# line 1032 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(';');


when 134 => 
--# line 1037 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('|');


when 135 => 
--# line 1042 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('?');


when 136 => 
--# line 1047 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('=');


  -- The following is used to match all valid Parython identifiers
  -- except reserved words. Note that leading digits and underscores
  -- are not allowed and that double underscores are not allowed.
when 137 => 
--# line 1057 "parython_lex.l"

	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Identifier);


  -- Enumeration literals
when 138 => 
--# line 1064 "parython_lex.l"

	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Enum_Literal);


  -- Decimal numeric literals
when 139 => 
--# line 1071 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);


when 140 => 
--# line 1077 "parython_lex.l"

      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);


  -- Based numeric literals.
when 141 => 
--# line 1085 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);


when 142 => 
--# line 1091 "parython_lex.l"

      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);


when 143 => 
--# line 1097 "parython_lex.l"

      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);


when 144 => 
--# line 1103 "parython_lex.l"

      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);


  -- Match all valid character literals.  See Ada LRM 2.6.
when 145 => 
--# line 1111 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Char_Literal);


  -- Match all valid string literals.  See Ada LRM 2.6.
when 146 => 
--# line 1119 "parython_lex.l"

	ECHO_L; ENTER(CHARLIT); 
        declare
           Tok : constant String := yytext;
           Quote : constant String := """";
        begin
           yylval := Create_Token (Tok);
           if Tok (Tok'Last) /= Quote(1) then
              yyerror ("unterminated string", At_Token => yylval);
           end if;
           return(String_Literal);
        end;


  -- Handle white space --
when 147 => 
--# line 1135 "parython_lex.l"
ECHO_L; -- ignore white space and comments to end-of-line

when 148 => 
--# line 1137 "parython_lex.l"
ECHO_L; -- ignore white space and comments to end-of-line

when 149 => 
--# line 1139 "parython_lex.l"

        --  Not start of line (or whole line); no need to keep track
        pragma Assert (Col_Count > 0);
        ECHO_L;


when 150 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1146 "parython_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 151 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1156 "parython_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 152 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1166 "parython_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 153 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1176 "parython_lex.l"

        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 154 => 
yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
 yy_cp := yy_bp + 1;
yy_c_buf_p := yy_cp;
YY_DO_BEFORE_ACTION; -- set up yytext again
--# line 1186 "parython_lex.l"

        --  Ignore outdent for label
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);


when 155 => 
--# line 1196 "parython_lex.l"

        --  Return another OUTDENT
        declare
           Do_Match_Check : Boolean := True;
        begin
           unput(yytext(1));
           if Top > 1 and then Col_Count < Indent_Stack(Top)
             and then (Col_Match_Expected
               or else Col_Count < Indent_Stack(Top-1))
           then
              --  Not yet reached a matching indent;
              --  Pop the stack
              if Debug_Indent then
                 Text_IO.Put_Line(" [popping" &
                   Integer'Image(Indent_Stack(Top)) &
                   " from Indent_Stack] ");
                 Text_IO.Flush;
              end if;
              Top := Top - 1;
              if Col_Count >= Indent_Stack(Top)
                or else not Col_Match_Expected
                or else Bracketing_Token_Stack(Top + 1) = ':'
                or else Bracketing_Token_Stack(Top + 1) = REFERS_TO
              then
                 --  Don't check for indent match this time;
                 --  wait til next time since only one outdent occurring,
                 --  or ":" or "=>" used at end of line (which allows multiple
                 --  outdents at a time).
                 Do_Match_Check := False;
              end if;
           end if;

           if Do_Match_Check then
              --  Found (nearly) matching indent
              if Debug_Indent then
                 Text_IO.Put_Line(" [exiting OUTDENTING] ");
                 Text_IO.Flush;
              end if;
              if Col_Count = 0 then
                 --  Return the token at the beginning of the line
                 ENTER(RESCANNING);
              else
                 ENTER(CHARLIT);
              end if;
              if Col_Count /= Indent_Stack(Top)
                and then Col_Match_Expected
              then
                 --  Not quite matching
                 Parser_Warning("indent mismatch; expected indent of" &
                   Integer'Image(Indent_Stack(Top)) & ", found" &
                   Integer'Image(Col_Count));

                 --  Force a match
                 --  TBD: Not sure this is the right choice
                 if Top = 1 then
                    Top := Top + 1;
                    if Debug_Indent then
                       Text_IO.Put_Line(" [pushing" &
                         Integer'Image(Col_Count) &
                         " on Indent_Stack] ");
                       Text_IO.Flush;
                    end if;
                 else
                    if Debug_Indent then
                       Text_IO.Put_Line(" [setting top of Indent_Stack to" &
                         Integer'Image(Col_Count) & "] "); Text_IO.Flush;
                    end if;
                 end if;
                 Indent_Stack(Top) := Col_Count;
              end if;
           end if;
           yylval := Create_Token(Text => "");
           if Do_Match_Check then
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning NEWLINE after OUTDENTs] ");
                 Text_IO.Flush;
              end if;
              return NEWLINE;
           else
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning OUTDENT] ");
                 Text_IO.Flush;
              end if;
              return OUTDENT;
           end if;
        end;


when YY_END_OF_BUFFER +CHARLIT + 1 
 |
YY_END_OF_BUFFER +TICK + 1 
 |
YY_END_OF_BUFFER +AFTER_IMPLEMENTS + 1 
 |
YY_END_OF_BUFFER +INITIAL + 1 
 =>
--# line 1284 "parython_lex.l"

        if Top = 0 then
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [EOF: indent off] "); Text_IO.Flush;
           end if;
           Expecting_Indent := False;
           if Debug_Indent then
              Text_IO.Put(" [returning EOF] "); Text_IO.Flush;
           end if;
           return End_Of_Input;
        else
           unput(YY_END_OF_BUFFER_CHAR);
           Col_Count := Indent_Stack(1);
           if Debug_Indent then
              Text_IO.Put_Line(" [popping" & Integer'Image(Indent_Stack(Top)) &
                " from Indent_Stack] ");
              Text_IO.Flush;
           end if;
           Top := Top - 1;
           if Top > 0 then
              if Debug_Indent then
                 Text_IO.Put_Line(" [entering EOF OUTDENTING] ");
                 Text_IO.Flush;
              end if;
              ENTER(OUTDENTING);
           else
              --  This is a significant NEWLINE
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning NEWLINE] ");
                 Text_IO.Flush;
              end if;
              yylval := Create_Token(Text => "");
              return NEWLINE;
           end if;
        end if;


when YY_END_OF_BUFFER +OUTDENTING + 1 
 =>
--# line 1321 "parython_lex.l"

        --  Return another OUTDENT
        unput(YY_END_OF_BUFFER_CHAR);
        if Debug_Indent then
           Text_IO.Put_Line(" [popping" & Integer'Image(Indent_Stack(Top)) &
             " from Indent_Stack] ");
           Text_IO.Flush;
        end if;
        Top := Top - 1;
        if Top = 0 then
           --  All done
           if Debug_Indent then
              Text_IO.Put_Line(" [exiting EOF OUTDENTING] ");
              Text_IO.Flush;
           end if;
           ENTER(CHARLIT);
        end if;

        yylval := Create_Token(Text => "");
        if Debug_Indent then
           Text_IO.Put_Line(" [returning EOF OUTDENT] ");
           Text_IO.Flush;
        end if;
        return OUTDENT;


  -- The following matches all new lines (and the preceding white space).
when 158 => 
--# line 1350 "parython_lex.l"

        --  TBD: Should we return NEWLINE if this line is entirely blank?
        if Echo_Input then
           text_io.new_line;
        end if;
        Display_Linenum;


  -- The following matches everything else and prints an error message
  -- indicating that something unexpected was found.
when 159 => 
--# line 1361 "parython_lex.l"
ECHO_L; 
	    yyerror("lexical error: '" &
	      Parython_Lex_DFA.yytext & "'");


when 160 => 
--# line 1366 "parython_lex.l"
raise AFLEX_SCANNER_JAMMED;
when YY_END_OF_BUFFER + RESCANNING + 1 => 
    return End_Of_Input;
                when YY_END_OF_BUFFER =>
                    -- undo the effects of YY_DO_BEFORE_ACTION
                    yy_ch_buf(yy_cp) := yy_hold_char;

                    yytext_ptr := yy_bp;

                    case yy_get_next_buffer is
                        when EOB_ACT_END_OF_FILE =>
                            begin
                            if ( yywrap ) then
                                -- note: because we've taken care in
                                -- yy_get_next_buffer() to have set up yytext,
                                -- we can now set up yy_c_buf_p so that if some
                                -- total hoser (like aflex itself) wants
                                -- to call the scanner after we return the
                                -- End_Of_Input, it'll still work - another
                                -- End_Of_Input will get returned.

                                yy_c_buf_p := yytext_ptr;

                                yy_act := YY_STATE_EOF((yy_start - 1) / 2);

                                goto do_action;
                            else
                                --  start processing a new file
                                yy_init := true;
                                goto new_file;
                            end if;
                            end;
                        when EOB_ACT_RESTART_SCAN =>
                            yy_c_buf_p := yytext_ptr;
                            yy_hold_char := yy_ch_buf(yy_c_buf_p);
                        when EOB_ACT_LAST_MATCH =>
                            yy_c_buf_p := yy_n_chars;
                            yy_current_state := yy_get_previous_state;

                            yy_cp := yy_c_buf_p;
                            yy_bp := yytext_ptr;
                            goto next_action;
                        when others => null;
                        end case; -- case yy_get_next_buffer()
                when others =>
                    text_io.put( "action # " );
                    text_io.put( INTEGER'IMAGE(yy_act) );
                    text_io.new_line;
                    raise AFLEX_INTERNAL_ERROR;
            end case; -- case (yy_act)
        end loop; -- end of loop waiting for end of file
end YYLex;
--# line 1366 "parython_lex.l"

end Parython_Lex;


