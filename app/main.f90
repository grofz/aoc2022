program main_aoc2022
  use day2201_mod, only: day2201
  use day2202_mod, only: day2202
  use day2203_mod, only: day2203
  use day2204_mod, only: day2204
  implicit none

  goto 03
  01 call day2201('inp/01/input.txt')
  02 call day2202('inp/02/input.txt')
  03 call day2203('inp/03/input.txt')
  04 call day2204('inp/04/input.txt')
end program main_aoc2022
