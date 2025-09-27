$MaxExtraPrecision=3100
flConst[x_] := Rationalize[SetPrecision[x, 60]]
FMA[x_, y_, z_] := x * y + z
Hypot[x_, y_] := Sqrt[x*x + y*y]
checkReal[a_] :=
  If[Or[Internal`RealValuedNumericQ[a], BooleanQ[a]],
    a,
    Throw["domain-error", BadValue]]
