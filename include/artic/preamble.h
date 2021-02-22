#ifndef ARTIC_PREAMBLE_H
#define ARTIC_PREAMBLE_H

static const char* preamble = R"(
trait Add[T] { fn add(T,T) -> T; }
trait Sub[T] { fn sub(T,T) -> T; }
trait Mul[T] { fn mul(T,T) -> T; }
trait Div[T] { fn div(T,T) -> T; }
trait Rem[T] { fn rem(T,T) -> T; }
trait LShift[T] { fn lshift(T,T) -> T; }
trait RShift[T] { fn rshift(T,T) -> T; }
trait And[T] { fn and(T,T) -> T; }
trait Or[T] { fn or(T,T) -> T; }
trait Xor[T] { fn xor(T,T) -> T; }
trait CmpLT[T] { fn lt(T,T) -> bool; }
trait CmpGT[T] { fn gt(T,T) -> bool; }
trait CmpLE[T] { fn le(T,T) -> bool; }
trait CmpGE[T] { fn ge(T,T) -> bool; }
trait CmpEq[T] { fn eq(T,T) -> bool; }
trait CmpNE[T] { fn ne(T,T) -> bool; }

trait Plus[T] { fn plus(T) -> T;}
trait Minus[T] { fn minus(T) -> T;}
trait Not[T] { fn not(T) -> T; }
trait Inc[T] { fn inc(T) -> T;}
trait Dec[T] { fn dec(T) -> T;}

trait FromInt[T] { fn from_int(u64) -> T; }
trait FromFloat[T] { fn from_float(f64) -> T; }
trait Bitcast[T, U] { fn bitcast(T) -> U; }

impl Add[i8] { #[import(cc = "builtin")] fn add(i8, i8) -> i8; }
impl Add[i16] { #[import(cc = "builtin")] fn add(i16, i16) -> i16; }
impl Add[i32] { #[import(cc = "builtin")] fn add(i32, i32) -> i32; }
impl Add[i64] { #[import(cc = "builtin")] fn add(i64, i64) -> i64; }
impl Add[u8] { #[import(cc = "builtin")] fn add(u8, u8) -> u8; }
impl Add[u16] { #[import(cc = "builtin")] fn add(u16, u16) -> u16; }
impl Add[u32] { #[import(cc = "builtin")] fn add(u32, u32) -> u32; }
impl Add[u64] { #[import(cc = "builtin")] fn add(u64, u64) -> u64; }
impl Add[f16] { #[import(cc = "builtin")] fn add(f16, f16) -> f16; }
impl Add[f32] { #[import(cc = "builtin")] fn add(f32, f32) -> f32; }
impl Add[f64] { #[import(cc = "builtin")] fn add(f64, f64) -> f64; }

impl Sub[i8] { #[import(cc = "builtin")] fn sub(i8, i8) -> i8; }
impl Sub[i16] { #[import(cc = "builtin")] fn sub(i16, i16) -> i16; }
impl Sub[i32] { #[import(cc = "builtin")] fn sub(i32, i32) -> i32; }
impl Sub[i64] { #[import(cc = "builtin")] fn sub(i64, i64) -> i64; }
impl Sub[u8] { #[import(cc = "builtin")] fn sub(u8, u8) -> u8; }
impl Sub[u16] { #[import(cc = "builtin")] fn sub(u16, u16) -> u16; }
impl Sub[u32] { #[import(cc = "builtin")] fn sub(u32, u32) -> u32; }
impl Sub[u64] { #[import(cc = "builtin")] fn sub(u64, u64) -> u64; }
impl Sub[f16] { #[import(cc = "builtin")] fn sub(f16, f16) -> f16; }
impl Sub[f32] { #[import(cc = "builtin")] fn sub(f32, f32) -> f32; }
impl Sub[f64] { #[import(cc = "builtin")] fn sub(f64, f64) -> f64; }

impl Mul[i8] { #[import(cc = "builtin")] fn mul(i8, i8) -> i8; }
impl Mul[i16] { #[import(cc = "builtin")] fn mul(i16, i16) -> i16; }
impl Mul[i32] { #[import(cc = "builtin")] fn mul(i32, i32) -> i32; }
impl Mul[i64] { #[import(cc = "builtin")] fn mul(i64, i64) -> i64; }
impl Mul[u8] { #[import(cc = "builtin")] fn mul(u8, u8) -> u8; }
impl Mul[u16] { #[import(cc = "builtin")] fn mul(u16, u16) -> u16; }
impl Mul[u32] { #[import(cc = "builtin")] fn mul(u32, u32) -> u32; }
impl Mul[u64] { #[import(cc = "builtin")] fn mul(u64, u64) -> u64; }
impl Mul[f16] { #[import(cc = "builtin")] fn mul(f16, f16) -> f16; }
impl Mul[f32] { #[import(cc = "builtin")] fn mul(f32, f32) -> f32; }
impl Mul[f64] { #[import(cc = "builtin")] fn mul(f64, f64) -> f64; }

impl Div[i8] { #[import(cc = "builtin")] fn div(i8, i8) -> i8; }
impl Div[i16] { #[import(cc = "builtin")] fn div(i16, i16) -> i16; }
impl Div[i32] { #[import(cc = "builtin")] fn div(i32, i32) -> i32; }
impl Div[i64] { #[import(cc = "builtin")] fn div(i64, i64) -> i64; }
impl Div[u8] { #[import(cc = "builtin")] fn div(u8, u8) -> u8; }
impl Div[u16] { #[import(cc = "builtin")] fn div(u16, u16) -> u16; }
impl Div[u32] { #[import(cc = "builtin")] fn div(u32, u32) -> u32; }
impl Div[u64] { #[import(cc = "builtin")] fn div(u64, u64) -> u64; }
impl Div[f16] { #[import(cc = "builtin")] fn div(f16, f16) -> f16; }
impl Div[f32] { #[import(cc = "builtin")] fn div(f32, f32) -> f32; }
impl Div[f64] { #[import(cc = "builtin")] fn div(f64, f64) -> f64; }

impl Rem[i8] { #[import(cc = "builtin")] fn rem(i8, i8) -> i8; }
impl Rem[i16] { #[import(cc = "builtin")] fn rem(i16, i16) -> i16; }
impl Rem[i32] { #[import(cc = "builtin")] fn rem(i32, i32) -> i32; }
impl Rem[i64] { #[import(cc = "builtin")] fn rem(i64, i64) -> i64; }
impl Rem[u8] { #[import(cc = "builtin")] fn rem(u8, u8) -> u8; }
impl Rem[u16] { #[import(cc = "builtin")] fn rem(u16, u16) -> u16; }
impl Rem[u32] { #[import(cc = "builtin")] fn rem(u32, u32) -> u32; }
impl Rem[u64] { #[import(cc = "builtin")] fn rem(u64, u64) -> u64; }
impl Rem[f16] { #[import(cc = "builtin")] fn rem(f16, f16) -> f16; }
impl Rem[f32] { #[import(cc = "builtin")] fn rem(f32, f32) -> f32; }
impl Rem[f64] { #[import(cc = "builtin")] fn rem(f64, f64) -> f64; }

impl LShift[i8] { #[import(cc = "builtin")] fn lshift(i8, i8) -> i8; }
impl LShift[i16] { #[import(cc = "builtin")] fn lshift(i16, i16) -> i16; }
impl LShift[i32] { #[import(cc = "builtin")] fn lshift(i32, i32) -> i32; }
impl LShift[i64] { #[import(cc = "builtin")] fn lshift(i64, i64) -> i64; }
impl LShift[u8] { #[import(cc = "builtin")] fn lshift(u8, u8) -> u8; }
impl LShift[u16] { #[import(cc = "builtin")] fn lshift(u16, u16) -> u16; }
impl LShift[u32] { #[import(cc = "builtin")] fn lshift(u32, u32) -> u32; }
impl LShift[u64] { #[import(cc = "builtin")] fn lshift(u64, u64) -> u64; }

impl RShift[i8] { #[import(cc = "builtin")] fn rshift(i8, i8) -> i8; }
impl RShift[i16] { #[import(cc = "builtin")] fn rshift(i16, i16) -> i16; }
impl RShift[i32] { #[import(cc = "builtin")] fn rshift(i32, i32) -> i32; }
impl RShift[i64] { #[import(cc = "builtin")] fn rshift(i64, i64) -> i64; }
impl RShift[u8] { #[import(cc = "builtin")] fn rshift(u8, u8) -> u8; }
impl RShift[u16] { #[import(cc = "builtin")] fn rshift(u16, u16) -> u16; }
impl RShift[u32] { #[import(cc = "builtin")] fn rshift(u32, u32) -> u32; }
impl RShift[u64] { #[import(cc = "builtin")] fn rshift(u64, u64) -> u64; }

impl And[i8] { #[import(cc = "builtin")] fn and(i8, i8) -> i8; }
impl And[i16] { #[import(cc = "builtin")] fn and(i16, i16) -> i16; }
impl And[i32] { #[import(cc = "builtin")] fn and(i32, i32) -> i32; }
impl And[i64] { #[import(cc = "builtin")] fn and(i64, i64) -> i64; }
impl And[u8] { #[import(cc = "builtin")] fn and(u8, u8) -> u8; }
impl And[u16] { #[import(cc = "builtin")] fn and(u16, u16) -> u16; }
impl And[u32] { #[import(cc = "builtin")] fn and(u32, u32) -> u32; }
impl And[u64] { #[import(cc = "builtin")] fn and(u64, u64) -> u64; }
impl And[bool] { #[import(cc = "builtin")] fn and(bool,bool) -> bool; }

impl Or[i8] { #[import(cc = "builtin")] fn or(i8, i8) -> i8; }
impl Or[i16] { #[import(cc = "builtin")] fn or(i16, i16) -> i16; }
impl Or[i32] { #[import(cc = "builtin")] fn or(i32, i32) -> i32; }
impl Or[i64] { #[import(cc = "builtin")] fn or(i64, i64) -> i64; }
impl Or[u8] { #[import(cc = "builtin")] fn or(u8, u8) -> u8; }
impl Or[u16] { #[import(cc = "builtin")] fn or(u16, u16) -> u16; }
impl Or[u32] { #[import(cc = "builtin")] fn or(u32, u32) -> u32; }
impl Or[u64] { #[import(cc = "builtin")] fn or(u64, u64) -> u64; }
impl Or[bool] { #[import(cc = "builtin")] fn or(bool,bool) -> bool; }

impl Xor[i8] { #[import(cc = "builtin")] fn xor(i8, i8) -> i8; }
impl Xor[i16] { #[import(cc = "builtin")] fn xor(i16, i16) -> i16; }
impl Xor[i32] { #[import(cc = "builtin")] fn xor(i32, i32) -> i32; }
impl Xor[i64] { #[import(cc = "builtin")] fn xor(i64, i64) -> i64; }
impl Xor[u8] { #[import(cc = "builtin")] fn xor(u8, u8) -> u8; }
impl Xor[u16] { #[import(cc = "builtin")] fn xor(u16, u16) -> u16; }
impl Xor[u32] { #[import(cc = "builtin")] fn xor(u32, u32) -> u32; }
impl Xor[u64] { #[import(cc = "builtin")] fn xor(u64, u64) -> u64; }
impl Xor[bool] { #[import(cc = "builtin")] fn xor(bool,bool) -> bool; }

impl CmpLT[i8] { #[import(cc = "builtin")] fn lt(i8, i8) -> bool; }
impl CmpLT[i16] { #[import(cc = "builtin")] fn lt(i16, i16) -> bool; }
impl CmpLT[i32] { #[import(cc = "builtin")] fn lt(i32, i32) -> bool; }
impl CmpLT[i64] { #[import(cc = "builtin")] fn lt(i64, i64) -> bool; }
impl CmpLT[u8] { #[import(cc = "builtin")] fn lt(u8, u8) -> bool; }
impl CmpLT[u16] { #[import(cc = "builtin")] fn lt(u16, u16) -> bool; }
impl CmpLT[u32] { #[import(cc = "builtin")] fn lt(u32, u32) -> bool; }
impl CmpLT[u64] { #[import(cc = "builtin")] fn lt(u64, u64) -> bool; }
impl CmpLT[f16] { #[import(cc = "builtin")] fn lt(f16, f16) -> bool; }
impl CmpLT[f32] { #[import(cc = "builtin")] fn lt(f32, f32) -> bool; }
impl CmpLT[f64] { #[import(cc = "builtin")] fn lt(f64, f64) -> bool; }

impl CmpGT[i8] { #[import(cc = "builtin")] fn gt(i8, i8) -> bool; }
impl CmpGT[i16] { #[import(cc = "builtin")] fn gt(i16, i16) -> bool; }
impl CmpGT[i32] { #[import(cc = "builtin")] fn gt(i32, i32) -> bool; }
impl CmpGT[i64] { #[import(cc = "builtin")] fn gt(i64, i64) -> bool; }
impl CmpGT[u8] { #[import(cc = "builtin")] fn gt(u8, u8) -> bool; }
impl CmpGT[u16] { #[import(cc = "builtin")] fn gt(u16, u16) -> bool; }
impl CmpGT[u32] { #[import(cc = "builtin")] fn gt(u32, u32) -> bool; }
impl CmpGT[u64] { #[import(cc = "builtin")] fn gt(u64, u64) -> bool; }
impl CmpGT[f16] { #[import(cc = "builtin")] fn gt(f16, f16) -> bool; }
impl CmpGT[f32] { #[import(cc = "builtin")] fn gt(f32, f32) -> bool; }
impl CmpGT[f64] { #[import(cc = "builtin")] fn gt(f64, f64) -> bool; }

impl CmpLE[i8] { #[import(cc = "builtin")] fn le(i8, i8) -> bool; }
impl CmpLE[i16] { #[import(cc = "builtin")] fn le(i16, i16) -> bool; }
impl CmpLE[i32] { #[import(cc = "builtin")] fn le(i32, i32) -> bool; }
impl CmpLE[i64] { #[import(cc = "builtin")] fn le(i64, i64) -> bool; }
impl CmpLE[u8] { #[import(cc = "builtin")] fn le(u8, u8) -> bool; }
impl CmpLE[u16] { #[import(cc = "builtin")] fn le(u16, u16) -> bool; }
impl CmpLE[u32] { #[import(cc = "builtin")] fn le(u32, u32) -> bool; }
impl CmpLE[u64] { #[import(cc = "builtin")] fn le(u64, u64) -> bool; }
impl CmpLE[f16] { #[import(cc = "builtin")] fn le(f16, f16) -> bool; }
impl CmpLE[f32] { #[import(cc = "builtin")] fn le(f32, f32) -> bool; }
impl CmpLE[f64] { #[import(cc = "builtin")] fn le(f64, f64) -> bool; }

impl CmpGE[i8] { #[import(cc = "builtin")] fn ge(i8, i8) -> bool; }
impl CmpGE[i16] { #[import(cc = "builtin")] fn ge(i16, i16) -> bool; }
impl CmpGE[i32] { #[import(cc = "builtin")] fn ge(i32, i32) -> bool; }
impl CmpGE[i64] { #[import(cc = "builtin")] fn ge(i64, i64) -> bool; }
impl CmpGE[u8] { #[import(cc = "builtin")] fn ge(u8, u8) -> bool; }
impl CmpGE[u16] { #[import(cc = "builtin")] fn ge(u16, u16) -> bool; }
impl CmpGE[u32] { #[import(cc = "builtin")] fn ge(u32, u32) -> bool; }
impl CmpGE[u64] { #[import(cc = "builtin")] fn ge(u64, u64) -> bool; }
impl CmpGE[f16] { #[import(cc = "builtin")] fn ge(f16, f16) -> bool; }
impl CmpGE[f32] { #[import(cc = "builtin")] fn ge(f32, f32) -> bool; }
impl CmpGE[f64] { #[import(cc = "builtin")] fn ge(f64, f64) -> bool; }

impl CmpEq[i8] { #[import(cc = "builtin")] fn eq(i8, i8) -> bool; }
impl CmpEq[i16] { #[import(cc = "builtin")] fn eq(i16, i16) -> bool; }
impl CmpEq[i32] { #[import(cc = "builtin")] fn eq(i32, i32) -> bool; }
impl CmpEq[i64] { #[import(cc = "builtin")] fn eq(i64, i64) -> bool; }
impl CmpEq[u8] { #[import(cc = "builtin")] fn eq(u8, u8) -> bool; }
impl CmpEq[u16] { #[import(cc = "builtin")] fn eq(u16, u16) -> bool; }
impl CmpEq[u32] { #[import(cc = "builtin")] fn eq(u32, u32) -> bool; }
impl CmpEq[u64] { #[import(cc = "builtin")] fn eq(u64, u64) -> bool; }
impl CmpEq[f16] { #[import(cc = "builtin")] fn eq(f16, f16) -> bool; }
impl CmpEq[f32] { #[import(cc = "builtin")] fn eq(f32, f32) -> bool; }
impl CmpEq[f64] { #[import(cc = "builtin")] fn eq(f64, f64) -> bool; }
impl CmpEq[bool] { #[import(cc = "builtin")] fn eq(bool,bool) -> bool; }

impl CmpNE[i8] { #[import(cc = "builtin")] fn ne(i8, i8) -> bool; }
impl CmpNE[i16] { #[import(cc = "builtin")] fn ne(i16, i16) -> bool; }
impl CmpNE[i32] { #[import(cc = "builtin")] fn ne(i32, i32) -> bool; }
impl CmpNE[i64] { #[import(cc = "builtin")] fn ne(i64, i64) -> bool; }
impl CmpNE[u8] { #[import(cc = "builtin")] fn ne(u8, u8) -> bool; }
impl CmpNE[u16] { #[import(cc = "builtin")] fn ne(u16, u16) -> bool; }
impl CmpNE[u32] { #[import(cc = "builtin")] fn ne(u32, u32) -> bool; }
impl CmpNE[u64] { #[import(cc = "builtin")] fn ne(u64, u64) -> bool; }
impl CmpNE[f16] { #[import(cc = "builtin")] fn ne(f16, f16) -> bool; }
impl CmpNE[f32] { #[import(cc = "builtin")] fn ne(f32, f32) -> bool; }
impl CmpNE[f64] { #[import(cc = "builtin")] fn ne(f64, f64) -> bool; }
impl CmpNE[bool] { #[import(cc = "builtin")] fn ne(bool,bool) -> bool; }

impl FromInt[i8] { fn from_int(x: u64) -> i8 = x as i8; }
impl FromInt[i16] { fn from_int(x: u64) -> i16 = x as i16; }
impl FromInt[i32] { fn from_int(x: u64) -> i32 = x as i32; }
impl FromInt[i64] { fn from_int(x: u64) -> i64 = x as i64; }
impl FromInt[u8] { fn from_int(x: u64) -> u8 = x as u8; }
impl FromInt[u16] { fn from_int(x: u64) -> u16 = x as u16; }
impl FromInt[u32] { fn from_int(x: u64) -> u32 = x as u32; }
impl FromInt[u64] { fn from_int(x: u64) -> u64 = x; }
impl FromInt[f16] { fn from_int(x: u64) -> f16 = x as f16; }
impl FromInt[f32] { fn from_int(x: u64) -> f32 = x as f32; }
impl FromInt[f64] { fn from_int(x: u64) -> f64 = x as f64; }

impl FromFloat[i8] { fn from_float(x: f64) -> i8 = x as i8; }
impl FromFloat[i16] { fn from_float(x: f64) -> i16 = x as i16; }
impl FromFloat[i32] { fn from_float(x: f64) -> i32 = x as i32; }
impl FromFloat[i64] { fn from_float(x: f64) -> i64 = x as i64; }
impl FromFloat[u8] { fn from_float(x: f64) -> u8 = x as u8; }
impl FromFloat[u16] { fn from_float(x: f64) -> u16 = x as u16; }
impl FromFloat[u32] { fn from_float(x: f64) -> u32 = x as u32; }
impl FromFloat[u64] { fn from_float(x: f64) -> u64 = x as u64; }
impl FromFloat[f16] { fn from_float(x: f64) -> f16 = x as f16; }
impl FromFloat[f32] { fn from_float(x: f64) -> f32 = x as f32; }
impl FromFloat[f64] { fn from_float(x: f64) -> f64 = x; }

impl Not[i8] { #[import(cc = "builtin")] fn not(i8) -> i8; }
impl Not[i16] { #[import(cc = "builtin")] fn not(i16) -> i16; }
impl Not[i32] { #[import(cc = "builtin")] fn not(i32) -> i32; }
impl Not[i64] { #[import(cc = "builtin")] fn not(i64) -> i64; }
impl Not[u8] { #[import(cc = "builtin")] fn not(u8) -> u8; }
impl Not[u16] { #[import(cc = "builtin")] fn not(u16) -> u16; }
impl Not[u32] { #[import(cc = "builtin")] fn not(u32) -> u32; }
impl Not[u64] { #[import(cc = "builtin")] fn not(u64) -> u64; }
impl Not[bool] { #[import(cc = "builtin")] fn not(bool) -> bool; }

#[allow_undecidable_impl]
impl[T] Plus[T] where FromInt[T], Add[T] { fn @plus(x: T) = 0 + x; }

#[allow_undecidable_impl]
impl[T] Minus[T] where FromInt[T], Sub[T] { fn @minus(x: T) = 0 - x; }

#[allow_undecidable_impl]
impl[T] Inc[T] where FromInt[T], Add[T] { fn @inc(x: T) = x + 1; }

#[allow_undecidable_impl]
impl[T] Dec[T] where FromInt[T], Sub[T]{ fn @dec(x: T) = x - 1; }

trait BitOps[T] where Not[T], And[T], Or[T], Xor[T];
trait Shift[T] where LShift[T], RShift[T];
trait Num[T] where Add[T], Sub[T], Mul[T], Div[T], Rem[T], FromInt[T];
trait Int[T] where Num[T], Shift[T], BitOps[T];
trait Float[T] where Num[T], FromFloat[T];

#[allow_undecidable_impl]
impl[T] BitOps[T] where Not[T], And[T], Or[T], Xor[T];
#[allow_undecidable_impl]
impl[T] Shift[T] where LShift[T], RShift[T];
#[allow_undecidable_impl]
impl[T] Num[T] where Add[T], Sub[T], Mul[T], Div[T], Rem[T], FromInt[T];
#[allow_undecidable_impl]
impl[T] Int[T] where Num[T], Shift[T], BitOps[T];
#[allow_undecidable_impl]
impl[T] Float[T] where Num[T], FromFloat[T];
)";


#endif // ARTIC_PREAMBLE_H
