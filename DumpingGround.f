c     Allows modularity, ie. Code still works after changing the line
c     "character*20 fileName" in the main program

c subroutine to SHOW parameters
      SUBROUTINE PrintParams(value)
      IMPLICIT double precision (A-Z)
      COMMON /Tcool/ dz,mdot, Cp
      print*, value
      END SUBROUTINE

      SUBROUTINE FluxLen(length, fileName)
      IMPLICIT NONE
      character*8 fileName
      integer length
c      integer length
      length =0
      open(21, file=fileName,status='old',err=30)
      DO
        read (21,*,end=30)
        length=length+1
      end DO
  30  close(21)
      END
      
      character*8 FUNCTION GetFileName()
      IMPLICIT NONE

c      character *8 fileName
      print*, "Please input the filename below:"
      read (5,*) GetFileName
      END FUNCTION
c End of all functions
      BLOCK DATA INIT
      double precision X0, X1 
      COMMON /RANGE/ X0, X1 
      DATA X0, X1 / 2.0, 6.0 / 
      END
c start of Main Program
      PROGRAM MAIN
      IMPLICIT NONE
      double precision pi, tau, array, x0,x1
      parameter(pi=3.14159265, tau=pi*2) ! general purpose parameters
c variables (unknown)
      character*8 finishTime_str, fileName
      integer finish(3),i, length
      dimension x0(1:10)
      x0 = (/(dble(i),i=1,10)/)
      DO i=1,10
            x0(i)=1
            print*,i,x0(i)
      end DO
      length=0
c      call FluxLen(length, fileName)
      !array = ReadFlux(fileName, length)
c VARIABLES (known)
c parameterS (known)
c DON'T TOUCH (to calculate the integer variables)

c Allocate memory for arrarys
c      double precision x
c      dimension x(1:XaryLeng)
c      x(:) = (/(i, i=XaryMi,XaryMa, XaryStep)/)/DBLE(XarySF)

c Actual calculations

c print program end time
 9999 call itime(finish)
      DO i=1,3
            write (6,*) ! write to screen 3 newline
            write(finishTime_str(3*i-2:3*i), "(I2.2,A)") finish(i),":" ! the last character will be discarded since it's out of range
      end DO
      print*, "__Program finished running at __ ", finishTime_str
      DO i=1,6
            write(6,*) "|"
      end DO
      END PROGRAM
c Parameter statement can only have  intrinsic functions inside of it