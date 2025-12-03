# MaxJolts ? JSize JoltList
MaxJolts ← |2 ⍣(
  ¤/↥ °1
  # | # ⊂⊙(MaxJolts -1):⟜(˜⊃⊡(↘+1)⍜↙⟜(⊢⊚⌕⊸/↥))+1-⊸⊙⧻
| ⊃(+1-⊙⧻|⋅∘|∘)
  ⍜↙⟜(⊢⊚⌕⊸/↥)
  ⊃(˜⊡|-1⋅⋅∘|˜(↘+1))
  ⊂⊙MaxJolts
)

&fras "input1"
⊜∘ ⊸≠@\n
/+≡(⋕MaxJolts 12)
