program main_aoc2022
  use day2201_mod, only: day2201
  use day2202_mod, only: day2202
  use day2203_mod, only: day2203
  implicit none

  goto 01
  01 call day2201('inp/01/input.txt')
  02 call day2202('inp/02/input.txt')
  03 call day2203('inp/03/input.txt')
end program main_aoc2022
