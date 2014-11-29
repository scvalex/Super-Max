open Core.Std

module Mat = struct
  type t = {
    m00 : float; m01 : float; m02 : float; m03 : float;
    m10 : float; m11 : float; m12 : float; m13 : float;
    m20 : float; m21 : float; m22 : float; m23 : float;
    m30 : float; m31 : float; m32 : float; m33 : float;
  } with fields, sexp, bin_io
end

module Vec = struct
  type t = {
    x : float; y : float; z : float; w : float;
  } with fields, sexp, bin_io
end
