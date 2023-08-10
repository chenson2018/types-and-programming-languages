pub const BUILTINS: &'static str = r"
let not = \a:Bool . if a then false else true;
let and = \a:Bool . \b:Bool . if a then b else false;
let or = \a:Bool . \b:Bool . if a then true else b;
NatList = Rec X . <nil:Unit, cons:{Nat, X}>;
NLBody = <nil:Unit, cons:{Nat,NatList}>;
let nil = fold [NatList] (<nil=unit> as NLBody);
let cons = \n:Nat . \l:NatList . fold [NatList] <cons={n, l}> as NLBody;

let isnil = 
	\l:NatList .
		case unfold [NatList] l of
			  <nil=x> → true
			| <cons=x> → false
;

let hd = 
	\l:NatList .
		case unfold [NatList] l of
			  <nil=x> → 0
			| <cons=x> → x.0
;

let tl = 
	\l:NatList .
		case unfold [NatList] l of
			  <nil=x> → l
			| <cons=x> → x.1
;

Rec_L2 = Rec A . A → (NatList → NatList → NatList);
Rec_N2 = Rec A . A → (Nat → Nat → Nat);
Rec_N1 = Rec A . A → (Nat → Nat);

let fix_list_airty_2 = 
	\f:(NatList → NatList → NatList)→(NatList → NatList → NatList) .
	(\x:Rec_L2 . f ((unfold [Rec_L2] x) x))
	(fold [Rec_L2] (\x:Rec_L2 . f ((unfold [Rec_L2] x) x)))
;

let fix_nat_airty_2 = 
	\f:(Nat → Nat → Nat)→(Nat → Nat → Nat) .
	(\x:Rec_N2 . f ((unfold [Rec_N2] x) x))
	(fold [Rec_N2] (\x:Rec_N2 . f ((unfold [Rec_N2] x) x)))
;

let fix_nat_airty_1 = 
	\f:(Nat → Nat)→(Nat → Nat) .
	(\x:Rec_N1 . f ((unfold [Rec_N1] x) x))
	(fold [Rec_N1] (\x:Rec_N1 . f ((unfold [Rec_N1] x) x)))
;

let plus = 
	fix_nat_airty_2 
	\f:Nat→Nat→Nat .
	\n:Nat .
	\m:Nat .
		if (iszero n)
		then
			m
		else
			f (pred n) (succ m)
;

let times = 
	fix_nat_airty_2
	\f:Nat→Nat→Nat .
	\n:Nat .
	\m:Nat .
		if or (iszero n) (iszero m)
		then
			0
		else
			if iszero (pred n)
			then
				m
			else
				plus m (f (pred n) m)
;

let pow = 
	fix_nat_airty_2
	\f:Nat→Nat→Nat .
	\n:Nat .
	\m:Nat .
	if (iszero n)
	then
		succ 0
	else
		if iszero (pred n)
		then
			m
		else
			times m (f (pred n) m)
;

let fact = 
	fix_nat_airty_1
	\f:Nat→Nat .
	\n:Nat .
	if or (iszero n) (iszero (pred n))
	then
		1
	else
		times n (f (pred n))
;

let rev_nat_list = 
 (
 fix_list_airty_2
 \f:NatList→NatList→NatList .
 \x:NatList .
 \y:NatList .
 		if isnil y
		then
			x
		else
			f (cons (hd y) x) (tl y)
	) nil
;
";
