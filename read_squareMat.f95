program readMat
implicit none 
real, allocatable, dimension(:,:):: mat1, mat2,result1,t_result1,t_mat1,t_mat2,result1_tt
integer:: size

!initialize matrices

print* , " Enter the size of the matrices:"
read*,size

!allocate(mat1(size,size),mat2(size,size),result1(size,size), t_result1(size,size),t_mat1(size,size),t_mat2(size,size),result1_tt(size,size))
allocate(result1_tt(size,size))
allocate(mat1(size,size),mat2(size,size),result1(size,size), t_result1(size,size),t_mat1(size,size),t_mat2(size,size))
print*,'Enter matrix A elements'
call fill_matrix(size,mat1)
print*, 'Enter matrix B elements'
call fill_matrix(size,mat2)
print* ,' Matrix A is :'
call  outputMat(size,mat1)
print*,' Matrix B is :'
call outputMat(size,mat2)
! multiply the two matrices 
!demonstrate the use of matmul and transpose intrinsic !functions
  result1=matmul(mat1,mat2)
  print *,'matmul of  mat1 and mat2'
  call  outputMat(size,result1)
  t_result1=transpose(result1)
  print *,'The tranpose of the two  matrices multilication  result '
  call  outputMat(size,t_result1)
  print*,'================== part 2 ============='
  t_mat1=transpose(mat1)
  print*,'Transpose of mat 1'
  call  outputMat(size,t_mat1)
!  print*,'Transpose of mat 2'
  t_mat2=transpose(mat2)
  print*,'Transpose of mat 2'
  call  outputMat(size,t_mat2)
 ! print*,'Multiplication result of the two tranposed matrices'
  result1_tt=matmul(t_mat2,t_mat1)
  print*,'Multiplication result of the two tranposed matrices'
  call  outputMat(size,result1_tt)
  print*,' Check fr equality between transposed of result of multiplication and multiplication of two transposed matrices'
  call  checkMat(t_result1,result1_tt,size)
deallocate(result1_tt)
deallocate(mat1,mat2,result1,t_result1,t_mat1,t_mat2)
end program readMat

subroutine fill_matrix(size,matr)
implicit none 
integer :: row, col , size
real :: num
real , dimension(size,size)::matr
do row=1,size
   do col=1,size
      print*,'Enter element at row ',row,'column : ',col
      read*,num
      matr(row,col)=num
    end do
 end do 
end subroutine fill_matrix

 subroutine  outputMat(size,matr)
  implicit none
  !will output a real square array nicely
  integer                               :: size,row,col
  real,dimension(size,size)             :: matr
 ! character                             :: reply*1
  do row =1,size
     write(*,10) (matr(row,col),col=1,size)
     10    format(100f10.2)
     !as we don't know how many numbers are to be output,
     !specify  !more than we need - the rest are ignored
  end do
!  print*,'__________________________________________________'
!  print*,'Hit a key and  press enter to continue'
!  read *,reply
  end subroutine  outputMat
  subroutine checkMat(matr1,matr2,size)
  implicit none 
  integer :: i,size,row,col,check
  real,dimension(size,size) :: matr1,matr2
  check = 0
    do i = 1,size
        if (matr1(i,size) /= matr2(i,size)) then
        check = check + 1
        print*,'Transposed Result of multiplied matrices and Result of two transposed multiplied matrices NOT Equal ! '
        exit
        end if
    end do 
    if(check==0) then 
    print*,'Yeah ! Transposed result is equal to the result of multiplied transposed'
    end if 
  end subroutine checkMat
