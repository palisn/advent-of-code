# Convert rotation into pos/neg number
RotationToNumber ← ⍣(¯⋕ °$"L_"|⋕ °$"R_")

&fras "input1"          # read files
⊜RotationToNumber ⊸≠@\n # split lines and convert
/+ =50 \(◿100+)         # scan for zeros mod 100
