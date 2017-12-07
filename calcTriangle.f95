program triangle
implicit none
integer, parameter :: ikind=selected_real_kind(p=15)
real (kind=ikind):: a1,b1,C1,area1,a2,b2,C2,area2,diff

print*,'Triangle 1'
call prompt(a1,b1,C1)
call calculate_area(a1,b1,C1,area1)
print*,'Triangle 2'
call prompt(a2,b2,C2)
call calculate_area(a2,b2,C2,area2)
print* ,'Triangle 1 Area = ',area1
print*, 'Triangle 2 Area =',area2
diff= area1-area2
print*,'The difference between the two is of ',diff


end program triangle

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine prompt(a,b,C)
implicit none
integer, parameter :: ikind=selected_real_kind(p=15)
real (kind=ikind):: a,b,C
print*,'Please enter the two side of the triangle 1:'
read *, a,b
print *,'Enter C as the degree of the Triangle:'
read*,C
end subroutine

subroutine calculate_area(a,b,C,area)
implicit none 
integer, parameter :: ikind=selected_real_kind(p=15)
real (kind=ikind):: a,b,C,area
area= abs(0.5*a*b*sin(C))
end subroutine
