# Convert rotation into pos/neg number
RotationToNumber ← ⍣(¯⋕ °$"L_"|⋕ °$"R_")

&fras "input1"        # read files
⊜□ ⊸≠@\n              # split lines
≡(RotationToNumber°□) # convert to numbers
/+ =50 \(◿100+)       # scan for zeros mod 100
