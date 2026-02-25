*****************************************************************
*     Listplot the 3D dispersion of the nodal-ring semimetals
*****************************************************************
      module value
      implicit double precision(a-h,o-z)
	real(8), parameter :: Pi=3.14159
      real(8), parameter :: xM=0.5            ! Dirac mass 
      end module value
cc
**********************
*     main program
**********************
      program main
	use value
      implicit double precision (a-h,o-z)
      open(10,file='11.dat')
      open(20,file='12.dat')
cc
      do 100 xk=-3,3,0.05
      do 100 yk=-3,3,0.05
         temp=(xk*xk+yk*yk-1)**2+xM**2
         en1=sqrt(temp)
         en2=-en1         
         if(abs(en1)<3) write(10,'(100f16.8)') xk,yk,en1
         if(abs(en2)<3) write(20,'(100f16.8)') xk,yk,en2 
100   continue      
cc
      end program main
cc
cc