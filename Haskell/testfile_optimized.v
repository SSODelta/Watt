module mux_2to1(Y, A, B, sel);
assign or__17__n_sel = (_17_ | (~ sel));
assign or__11__n_A = (_11_ | (~ A));
assign or__12__n_B = (_12_ | (~ B));
assign or__13__n_A = (_13_ | (~ A));
assign or__14__n_B = (_14_ | (~ B));
assign or__15__n__2_ = (_15_ | (~ _2_));
assign or__16__n__1_ = (_16_ | (~ _1_));
assign or__09__and_sel__0_ = (_09_ | (sel & _0_));
assign or__10__and__17___2_ = (_10_ | (_17_ & _2_));
assign or__03__and__11__B = (_03_ | (_11_ & B));
assign or__04__and_A__12_ = (_04_ | (A & _12_));
assign or__05__and__13__B = (_05_ | (_13_ & B));
assign or__06__and_A__14_ = (_06_ | (A & _14_));
assign or__07__and__15___1_ = (_07_ | (_15_ & _1_));
assign or__08__and__2___16_ = (_08_ | (_2_ & _16_));
endmodule
