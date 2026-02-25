*****************************************************
*     Calculate the LLs of a nodal-ring semimetal 
*****************************************************
      module value
      implicit double precision (a-h,o-z)      
      real(8), parameter :: Pi=3.14159 
      real(8), parameter :: xM=0.5          ! Dirac mass 
      real(8), parameter :: epsilont=sqrt(1+xM**2)  ! transition energy
cc      
      end module value
cc      
**********************
*     main program
**********************
      program main
      use value
      implicit double precision (a-h,o-z)
      character(100) filename 
      write(filename,'(a7)') 'Res.dat'
      open(10,file=filename)         
cc 
      do 100 epsilon=0,5,0.001
      DOSa=0                             ! DOS 
      DOSb=0
      xn0a=0                             ! carrier density
      xn0b=0
      if(epsilon>xM) then
        aa=epsilon/(4*Pi*Pi*sqrt(epsilon**2-xM**2))   
        bb=6*Pi*Pi
        DOSa=sqrt(1+sqrt(epsilon**2-xM**2))*aa 
        xn0a=(sqrt(epsilon**2-xM**2)+1)**1.5 
        xn0a=xn0a/bb
cc        
        if(epsilon<epsilont) then
           DOSb=sqrt(1-sqrt(epsilon**2-xM**2))*aa
           xn0b=(1-sqrt(epsilon**2-xM**2))**1.5 
           xn0b=xn0b/bb 
        end if    
      end if          
      DOS=DOSa+DOSb 
      xn0=xn0a-xn0b 
      write(10,*) epsilon,DOS,xn0  
cc 
100   continue          
cc      
      end program main
cc