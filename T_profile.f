c By Ocean Wong, 2018-12-25 17:55:08, Unversity of Birmingham, PTNR, as a FORTRAN assignment
      PROGRAM MAIN
      IMPLICIT NONE
c Defining variables
c zero-dimension variables (unknown)
      integer length, arysize, i,o, str_max_len,fluxsize
      parameter(str_max_len=20,fluxsize=102)!Allocate memory for fileName
      
      character*(str_max_len) fileName
      character*(str_max_len+9) outFile, OutFileName
      character*4 C !only requried for special character processing purpose.
      double precision Init_params

c one-dimensional varibales: temperature and flux arrays
      parameter(arysize=1023) !Allocate fixed amount of memory for array
      double precision Tcool, Tclado, Tcladi, Tfuelo, Tfmax, flux
      double precision q_lin_over_pi,z
      dimension Tcool(1:arysize+1), Tclado(1:arysize), Tcladi(1:arysize)
      dimension Tfuelo(1:arysize), Tfmax(1:arysize), flux(1:arysize)
      dimension q_lin_over_pi(1:arysize), z(1:arysize+1)

      call PrintProgramHeader
c Ask for inputs parameters
      call GetFileName(fileName)
      call ScanFlux(fileName,length, flux,arysize) !Set the flux distribution
      write(6,'(A,I4,A)') "Successfully read ", length, " lines."
      print*,
      o=21
      C='('//char(176)//'C)' !Degree C in brackets
      outFile=trim(OutFileName(fileName))
      open(o,file=outFile,access='sequential',action='write')
c Choose reactor type, set inlet coolant temperatures + other global parameters
      Tcool(1)= Init_params(length)
      write(o,60)"height(m)","T cool", C,"T clad_o", C, "T max",C !Write to file
      write(6,60)"height(m)","T cool",'(C)',"Toc",'(C)',"T max",'(C)' !Write to screen
  60  format(A,T15,A,A,T30,A,A,T45,A,A)
      DO i=1,length
          call IncrementHeight(    z(i),   z(i+1)                 )
c Declaration statement already accounted for extra memory needed
c to avoid seg fault at full capacity
          call LinearHeatGenOverPi(flux(i),q_lin_over_pi(i)       )
          call HeatCoolant( Tcool(i),Tcool(i+1), q_lin_over_pi(i) )
          call CoolCladding(   Tcool(i),Tclado(i),q_lin_over_pi(i))
          call CoolCladdingIn(Tclado(i),Tcladi(i),q_lin_over_pi(i))
          call CoolFuel(      Tcladi(i),Tfuelo(i),q_lin_over_pi(i))
          call FindHottest(   Tfuelo(i), Tfmax(i),q_lin_over_pi(i))
          call SaveTemps(z(i), Tcool(i),Tclado(i),Tfmax(i), o     )
      end DO
      close(o)
      write(6,*)"The above data has been saved to ",outFile
      call finished() !print program end time
      END PROGRAM
c End of main functions_________________________________________________
      


c Functions and subroutines_____________________________________________
c (in the order that has appeared in the main program)

      SUBROUTINE PrintProgramHeader()
      IMPLICIT NONE
      print*, "By Ocean Wong, Unversity of Birmingham"
      write(*,*) "Program to find temperature profiles of coolant,"//
     >"cladding, and fuel in AGR/PWR given a text file of flux profile."
      print*, !print a newline
      END SUBROUTINE

c subroutine to prompt for file name and handle error
      SUBROUTINE GetFileName(fileName)
      IMPLICIT NONE
      character(len=*) fileName !This is an accepted F77 syntax; and is used because
c it allows modularity, i.e. max length of fileName in the main program can be modified
      integer ios
      logical ex
      print*, "Please enter the file name without '.txt',"
      print*, "enter nothing to use the default value for file name"//
     >"(flux.txt)"
  50  read(5,*,IOSTAT=ios) fileName
      fileName=trim(fileName)//'.txt'
      inquire(file=fileName, EXIST=ex) !check that file exist
      IF ((.NOT.ex).or.(ios.NE.0)) then
          write(*,'(A,A,A)') trim(fileName)," doesn't exist, "//
     >"please re-enter file name(without .txt):"
          GO TO 50 !ask for file name again.
      end IF
      END SUBROUTINE

c subroutine that reads the flux value from file and errors
      SUBROUTINE ScanFlux(fileName, length, flux, arysize)
      IMPLICIT NONE
      character(len=*) fileName ! allow 
      integer length,len,arysize !arysize is already parametrised in the main program
      double precision flux
      dimension flux(1:arysize)
      length=0
      open(10, file=fileName,status='old',err=20)!exit MAIN program if error occurs

      DO while (length.LE.(size(flux)))
          read(10,*,end=30,err=20) flux(length+1)
c         If end is reached successfully, the file will be closed, skipping to line 30
          length=length+1
      end DO
      !Will only redirect to this part if file is larger than memory allowance.
      write(6,'(A,I7,A)')"Input file larger than allocated memory = ",
     >size(flux), " values."
      length = length-1 !Done so that
      GO TO 20

      !If error occurs:
  20  write(6,'(A,A,A)') "Unable to read ", trim(fileName), '!'
      write(6,'(A,I4)') "Error encountered while reading line",
     >(length+1)
      print *, "Exiting..."
      STOP !exit the MAIN program
  
  30  close(10)
      END SUBROUTINE

c subprogram to prompt for reactor type
      FUNCTION GetReactorType()
      IMPLICIT NONE
      character*3 GetReactorType
      integer ios
      print*, "Please specify whether this is an 'AGR' or a 'PWR':"
      read(5,*, IOSTAT=ios) GetReactorType
      DO WHILE (((GetReactorType/="AGR").AND.(GetReactorType/="PWR")
     >).OR.(ios.NE.0))!is neither AGR or PWR
          print*, "Option not recognized, please retry('AGR'/'PWR')"
          read(5,*, IOSTAT=ios) GetReactorType
      end DO
      END FUNCTION

c Function to modify input file name to get the output filename.
      FUNCTION OutFileName(fileName)
      IMPLICIT NONE
      character(len=*) fileName,OutFileName
      integer str_len
      logical ex
      character*1 YN
      str_len = len(trim(fileName))-4
      OutFileName=fileName(1:str_len)//"_T_output.txt"
      write(*,'(A)') "Temperature data will be saved to "//OutFileName
      !warn user of potential overwriting
      inquire(file=trim(OutFileName),EXIST=ex)
      IF (ex) then
  70      write(*,'(A)') "Overwrite? (y/n)"
          read(5,*) YN
          IF ((YN=='N').OR.(YN=='n')) then
              STOP
          ELSEIF((YN/='Y').AND.(YN/='y')) then
              GO TO 70 !Go back to prompt again
          !Only proceed forward if Answer = 'y' or 'Y')
          end IF
      end IF
      END FUNCTION

c Function to initialize the operating parameters of the reactor
      FUNCTION Init_params(len_flux)
      IMPLICIT double precision (A-Z)
      integer len_flux
      character*3 ReactorType, GetReactorType
      logical InputCorrect
c make 1 d array here:
      COMMON /vol_to_lin/ factor
      ! factor to convert flux into linear heat generation rate
      COMMON /Tcool/ dz, mdot, Cp
      COMMON /Toc/   hc, dc
      COMMON /Tic/   kc, ln_d_c_over_d_f
      COMMON /Tof/   hg, df
      COMMON /Tfmax/ kf, dh2_df2_dh2_ln_df_dh
      ! initialize kf, macroscopic cross section and Q simulatneously
      ReactorType = GetReactorType()
      !Initialize other parameters
      IF (ReactorType=='AGR') then
          call AGRparameters(zmax,mdot,Cp,hc,Tin,E,df,dh,Tc,kc,hg)
      ELSEIF (ReactorType=='PWR') then
          call PWRparameters(zmax,mdot,Cp,hc,Tin,E,df,dh,Tc,kc,hg)
      end IF
      call UO2MaterialConstants(kf,Sigma_mac,Q,E)!E=fraction of U-235
      ! processing to get intermediate constants
      dz=zmax/(len_flux-1) !step size in the vertical direction
      A_f_pi=(df**2-dh**2)/4 !Fuel area divided by pi
      factor = Sigma_mac*Q*A_f_pi!factor to convert flux into q'''*A_f/pi
      dc = Tc*2+df !diameter of the cladding
      ln_d_c_over_d_f = log(dc/df)
      IF (dh.NE.0.0) then
          dh2_df2_dh2_ln_df_dh= dh**2/(df**2-dh**2)*log(df/dh)
      ELSE
          dh2_df2_dh2_ln_df_dh=0 !avoids division by zero inside the logarithm
c This has the effect of making the last term in the equation for T_max vanish,
c which is true when d_h ->0.
      end IF
      Init_params=Tin !set the inlet coolant temperature
      END FUNCTION

c The following 3 subroutines (*parameters and UO2MatConsts) were used in Init_params.
c Everything below are in SI units, and the layout follows that of the question sheet.
c AGR parameters, placed in a separate subroutine for clarity
      SUBROUTINE AGRparameters(zmax,mdot,Cp,hc,Tin,E,df,dh,Tc,kc,hg)
      IMPLICIT double precision(A-Z)
      zmax= 8.0
      mdot= 0.35
      Cp  = 1120.0
      hc  = 1000.0
      Tin = 339.0

      E   = 2.0E-2
      df  = 14.5E-3
      dh  = 6.0E-3

      Tc  = 0.38E-3!thickness of cladding
      kc  = 18.0
      hg  = 4000.0
      END SUBROUTINE
c PWR parameters, placed in a separate subroutine for clairty
      SUBROUTINE PWRparameters(zmax,mdot,Cp,hc,Tin,E,df,dh,Tc,kc,hg) !In SI units
      IMPLICIT double precision(A-Z)
      zmax= 4.0
      mdot= 0.35
      Cp  = 5500.0
      hc  = 33000.0
      Tin = 292.0

      E   = 4.0E-2
      df  = 9.0E-3
      dh  = 0.0

      Tc = 0.5E-3!thickness of cladding
      kc = 14.0
      hg = 4000.0
      END SUBROUTINE
c UO2 Material parameters, placed in a separate subroutine for clairty
      SUBROUTINE UO2MaterialConstants(k_f,Macroscopic,Q,E)
      IMPLICIT double precision (A-Z)
      parameter(eV=1.602e-19, N_A=6.022e+23)
      !         eV in Joule, Avogadro's constant
      k_f = 2.65
      rho = 10.97E3
      sigma_mic=580E-28!microscopic fission cross section
      Q=200E6*eV
      !A= effective molecular mass of each UO2 atom
      A=((1.0-E)*(238.0+32.0)+E*(235.0+32.0))*1E-3
      !A is expected to vary wrt. enrichment E as above,
      
      !An alternative expression is listed below,
      !A = (238.0+32.0) !Of each UO2
      !where it is approximated to be constant irrespective of the enrichment level.
      Macroscopic=sigma_mic*E*N_A*rho/A !Macroscopic fission cross section
      END SUBROUTINE

c subroutine to calculate various z- dependent variables
c subroutine to calculate height
      SUBROUTINE IncrementHeight(z,z1)
      IMPLICIT double precision(A-Z)
      COMMON /Tcool/ dz,dummy,dummy2
      z1=z+dz
      END SUBROUTINE

c subroutine to convert flux into linear heat genration rate
      SUBROUTINE LinearHeatGenOverPi(flux_i, lin_heat_i)
      IMPLICIT double precision (A-Z)
      COMMON /vol_to_lin/ factor
      lin_heat_i = flux_i*factor
      END SUBROUTINE

c qi in the following represents linear heat generation rate divided by pi.
      SUBROUTINE HeatCoolant(Tc,Tc1,qi)
      !Tc1 = Tcoolant at (z+dz); Tc = Tcoolant at Tcool(z)
      IMPLICIT double precision(A-Z)
      PARAMETER(pi=3.14159265)
      COMMON /Tcool/ dz,mdot,Cp
      Tc1 = Tc+ qi*pi*dz/mdot/Cp
      END SUBROUTINE

      SUBROUTINE CoolCladding(Tc,Tco,qi)
      IMPLICIT double precision(A-Z)
      COMMON /Toc/   hc, dc
      Tco = Tc+qi/hc/dc
      END SUBROUTINE

      SUBROUTINE CoolCladdingIn(Tco,Tci,qi)
      IMPLICIT double precision(A-Z)
      COMMON /Tic/   kc, ln_d_c_over_d_f
      Tci = Tco + qi/(2.0*kc)*ln_d_c_over_d_f
      END SUBROUTINE

      SUBROUTINE CoolFuel    (Tci,Tof,qi)
      IMPLICIT double precision(A-Z)
      COMMON /Tof/   hg, df
      Tof= Tci+qi/hg/df
      END SUBROUTINE

      SUBROUTINE FindHottest (Tof,Tmax,qi)
      IMPLICIT double precision(A-Z)
      COMMON /Tfmax/ kf, dh2_df2_dh2_ln_df_dh
      Tmax=Tof+qi/kf/2.0*( 0.5 - dh2_df2_dh2_ln_df_dh )
      END SUBROUTINE
c Subroutine to save the temperatures to file + print to screen
      SUBROUTINE SaveTemps(z, T1, T2, T3,chn)
      IMPLICIT double precision(A-Z)
      integer chn
      write(chn,'(F5.3,T15,F7.2,T30,F7.2,T45,F7.2)') z,T1,T2,T3
      write(6  ,'(F5.3,T15,F7.2,T30,F7.2,T45,F7.2)') z,T1,T2,T3
      END SUBROUTINE

c subroutine to output completion time at the bottom
      SUBROUTINE finished()
      IMPLICIT NONE
      character*8 finishTime_str
      integer finish(3), i
      call itime(finish)
      DO i=1,3
            write (6,*) ! write to screen 3 newline
            write(finishTime_str(3*i-2:3*i), "(I2.2,A)") finish(i),":" 
      ! The last character will be discarded since it's out of range
      end DO
      print*, "__Program finished running at __ ", finishTime_str
      DO i=1,6
            write(6,*) "|" !Improves readability of the terminal output.
      end DO
      END SUBROUTINE