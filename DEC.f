      SUBROUTINE DFLUX(FLUX,SOL,KSTEP,KINC,TIME,NOEL,NPT,COORDS,
     1 JLTYP,TEMP,PRESS,SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION FLUX(2), TIME(2), COORDS(2)
      CHARACTER*80 SNAME
      
      x=COORDS(1)
      y=COORDS(2)
      z=COORDS(3)
      t=TIME(2)
      temp=SOL
	
      pi=3.141592654
      eulers=2.718281828459



	
      t_2nd_pass=502.2	!End of first pass i.e. start of second pass
      t_3rd_pass=1098.4
      t_4th_pass=1831.8
      t_5th_pass=2689.2
      t_6th_pass=3657
      t_7th_pass=4536.4
      t_8th_pass=5410
      t_9th_pass=6313.6
      t_10th_pass=7260.4
      t_11th_pass=8315
      t_12th_pass=9291.4
      t_13th_pass=10357.2
      t_14th_pass=11478.4
      t_15th_pass=12536
      t_16th_pass=13607.6
      t_17th_pass=14820.8
      t_18th_pass=15853.2
      t_19th_pass=17031
      t_20th_pass=18148.6
      t_21th_pass=19288.8
      t_22th_pass=20399.8
      t_23th_pass=21526.8
      t_24th_pass=22656.6
      t_25th_pass=23803.4


C
      a_e_cond=5.0 ! a heat flux parameter at dg
      a_i_cond=5.1  ! a heat flux parameter at yi
      
      b_cond=3.0

      cr_e_cond=4.0 ! cr heat flux parameter at dg
      cf_e_cond=1.0 ! cf heat flux parameter at dg
      
      cr_i_cond=6.0 ! cr heat flux parameter at yi
      cf_i_cond=2.0 ! cf heat flux parameter at yi
      

      
      d_g=28.0
      y_i=d_g-6.0


      frequency=0.5	!Number of oscillations per second
      Amplitude=8.4

      b_g=72.5-(Amplitude*sin(2*pi*frequency*t))

      if(b_g.GE.(72.5+7.5))THEN
	b_g=72.5+7.5
      endif

      if(b_g.LE.(72.5-7.5))THEN
	b_g=72.5-7.5
      endif


      
      d_g_2nd_pass=26.88
      d_g_3rd_pass=25.76
      d_g_4th_pass=24.64
      d_g_5th_pass=23.52
      d_g_6th_pass=22.4
      d_g_7th_pass=21.28
      d_g_8th_pass=20.16
      d_g_9th_pass=19.04
      d_g_10th_pass=17.92
      d_g_11th_pass=16.8
      d_g_12th_pass=15.68
      d_g_13th_pass=14.56
      d_g_14th_pass=13.44
      d_g_15th_pass=12.32
      d_g_16th_pass=11.2
      d_g_17th_pass=10.08
      d_g_18th_pass=8.96
      d_g_19th_pass=7.84
      d_g_20th_pass=6.72
      d_g_21th_pass=5.6
      d_g_22th_pass=4.48
      d_g_23th_pass=3.36
      d_g_24th_pass=2.24
      d_g_25th_pass=1.12
      
      
      y_i_2nd_pass=d_g_2nd_pass-6.0
      y_i_3rd_pass=d_g_3rd_pass-6.0
      y_i_4th_pass=d_g_4th_pass-6.0
      y_i_5th_pass=d_g_5th_pass-6.0
      y_i_6th_pass=d_g_6th_pass-6.0
      y_i_7th_pass=d_g_7th_pass-6.0
      y_i_8th_pass=d_g_8th_pass-6.0
      y_i_9th_pass=d_g_9th_pass-6.0
      y_i_10th_pass=d_g_10th_pass-6.0
      y_i_11th_pass=d_g_11th_pass-6.0
      y_i_12th_pass=d_g_12th_pass-6.0
      y_i_13th_pass=d_g_13th_pass-6.0
      y_i_14th_pass=d_g_14th_pass-6.0
      y_i_15th_pass=d_g_15th_pass-6.0
      y_i_16th_pass=d_g_16th_pass-6.0
      y_i_17th_pass=d_g_17th_pass-6.0
      y_i_18th_pass=d_g_18th_pass-6.0
      y_i_19th_pass=d_g_19th_pass-6.0
      y_i_20th_pass=d_g_20th_pass-6.0
      y_i_21th_pass=d_g_21th_pass-6.0
      y_i_22th_pass=d_g_22th_pass-6.0
      y_i_23th_pass=d_g_23th_pass-6.0
      y_i_24th_pass=d_g_24th_pass-6.0
      y_i_25th_pass=d_g_25th_pass-6.0


      Voltage=11.2
      Current=175.0
      eff=2.58122514561157
   


      Current_2=190.0
      Current_3=220.0
      Current_4=275.0
      Current_5=275.0
      Current_6=275.0
      Current_7=275.0
      Current_8=300
      Current_9=325.0
      Current_10=325.0
      Current_11=325.0
      Current_12=325.0
      Current_13=325.0
      Current_14=325.0
      Current_15=350.0
      Current_16=375.0
      Current_17=325.0
      Current_18=325.0
      Current_19=350.0
      Current_20=350.0
      Current_21=325.0
      Current_22=325.0
      Current_23=325.0
      Current_24=325.0
      Current_25=325.0


      varc=1.25

      
      
      z_start=85
      z_start_first=z_start


							
      IF(t.GT.t_2nd_pass)THEN
      		z_start=-(varc*t_2nd_pass)+z_start_first
		Current=Current_2
		d_g=d_g_2nd_pass
      		y_i=y_i_2nd_pass
		eff=1.17880671710359
      ENDIF
      IF(t.GT.t_3rd_pass)THEN
      		z_start=-(varc*t_3rd_pass)+z_start_first
		Current=Current_3
      		d_g=d_g_3rd_pass
		y_i=y_i_3rd_pass
		eff=0.86180078335718
      ENDIF
      IF(t.GT.t_4th_pass)THEN
      		z_start=-(varc*t_4th_pass)+z_start_first
		Current=Current_4
      		d_g=d_g_4th_pass
		y_i=y_i_4th_pass
		eff=0.64275039152808
      ENDIF
      IF(t.GT.t_5th_pass)THEN
      		z_start=-(varc*t_5th_pass)+z_start_first
		Current=Current_5
      		d_g=d_g_5th_pass
		y_i=y_i_5th_pass
      ENDIF
      IF(t.GT.t_6th_pass)THEN
      		z_start=-(varc*t_6th_pass)+z_start_first
		Current=Current_6
      		d_g=d_g_6th_pass
		y_i=y_i_6th_pass
      ENDIF
      IF(t.GT.t_7th_pass)THEN
      		z_start=-(varc*t_7th_pass)+z_start_first
		Current=Current_7
      		d_g=d_g_7th_pass
		y_i=y_i_7th_pass
      ENDIF
      IF(t.GT.t_8th_pass)THEN
      		z_start=-(varc*t_8th_pass)+z_start_first
		Current=Current_8
      		d_g=d_g_8th_pass
		y_i=y_i_8th_pass
      ENDIF
      IF(t.GT.t_9th_pass)THEN
      		z_start=-(varc*t_9th_pass)+z_start_first
		Current=Current_9
      		d_g=d_g_9th_pass
		y_i=y_i_9th_pass
      ENDIF
      IF(t.GT.t_10th_pass)THEN
      		z_start=-(varc*t_10th_pass)+z_start_first
		Current=Current_10
      		d_g=d_g_10th_pass
		y_i=y_i_10th_pass
      ENDIF
      IF(t.GT.t_11th_pass)THEN
      		z_start=-(varc*t_11th_pass)+z_start_first
		Current=Current_11
      		d_g=d_g_11th_pass
		y_i=y_i_11th_pass
      ENDIF
      IF(t.GT.t_12th_pass)THEN
      		z_start=-(varc*t_12th_pass)+z_start_first
		Current=Current_12
      		d_g=d_g_12th_pass
		y_i=y_i_12th_pass
      ENDIF
      IF(t.GT.t_13th_pass)THEN
      		z_start=-(varc*t_13th_pass)+z_start_first
		Current=Current_13
      		d_g=d_g_13th_pass
		y_i=y_i_13th_pass
      ENDIF
      IF(t.GT.t_14th_pass)THEN
      		z_start=-(varc*t_14th_pass)+z_start_first
		Current=Current_14
      		d_g=d_g_14th_pass
		y_i=y_i_14th_pass
      ENDIF
      IF(t.GT.t_15th_pass)THEN
      		z_start=-(varc*t_15th_pass)+z_start_first
		Current=Current_15
      		d_g=d_g_15th_pass
		y_i=y_i_15th_pass
      ENDIF
      IF(t.GT.t_16th_pass)THEN
      		z_start=-(varc*t_16th_pass)+z_start_first
		Current=Current_16
      		d_g=d_g_16th_pass
		y_i=y_i_16th_pass
      ENDIF
      IF(t.GT.t_17th_pass)THEN
      		z_start=-(varc*t_17th_pass)+z_start_first
		Current=Current_17
      		d_g=d_g_17th_pass
		y_i=y_i_17th_pass
		eff=1.0
      ENDIF
      IF(t.GT.t_18th_pass)THEN
      		z_start=-(varc*t_18th_pass)+z_start_first
		Current=Current_18
      		d_g=d_g_18th_pass
		y_i=y_i_18th_pass
		eff=1.0
      ENDIF
      IF(t.GT.t_19th_pass)THEN
      		z_start=-(varc*t_19th_pass)+z_start_first
		Current=Current_19
      		d_g=d_g_19th_pass
		y_i=y_i_19th_pass
		eff=1.0
      ENDIF
      IF(t.GT.t_20th_pass)THEN
      		z_start=-(varc*t_20th_pass)+z_start_first
		Current=Current_20
      		d_g=d_g_20th_pass
		y_i=y_i_20th_pass
		eff=1.0
      ENDIF
      IF(t.GT.t_21th_pass)THEN
      		z_start=-(varc*t_21th_pass)+z_start_first
		Current=Current_21
      		d_g=d_g_21th_pass
		y_i=y_i_21th_pass
		eff=1.0
      ENDIF
      IF(t.GT.t_22th_pass)THEN
      		z_start=-(varc*t_22th_pass)+z_start_first
		Current=Current_22
      		d_g=d_g_22th_pass
		y_i=y_i_22th_pass
		eff=1.0
      ENDIF
      IF(t.GT.t_23th_pass)THEN
      		z_start=-(varc*t_23th_pass)+z_start_first
		Current=Current_23
      		d_g=d_g_23th_pass
		y_i=y_i_23th_pass
		eff=1.0
      ENDIF
      IF(t.GT.t_24th_pass)THEN
      		z_start=-(varc*t_24th_pass)+z_start_first
		Current=Current_24
      		d_g=d_g_24th_pass
		y_i=y_i_24th_pass
		eff=1.0
      ENDIF
      IF(t.GT.t_25th_pass)THEN
      		z_start=-(varc*t_25th_pass)+z_start_first
		Current=Current_25
      		d_g=d_g_25th_pass
		y_i=y_i_25th_pass
		eff=1.0
      ENDIF




      zloc=z-(varc*t)-z_start
      yloc=y-d_g
      xloc=x-b_g

 
      R_DE_front=2/(1+(cr_e_cond/cf_e_cond)+((((eulers**3)-1)
     1 *sqrt(pi/3.0)*((cf_e_cond*((2.0*a_e_cond)+a_i_cond))+(cf_i_cond*
     2 ((2.0*a_i_cond)+a_e_cond)))*(d_g-y_i))/(3.0*b_cond*cf_e_cond*
     3 (eulers**3)*a_e_cond))+((((eulers**3)-1)*sqrt(pi/3.0)*((cr_e_cond*
     4 ((2.0*a_e_cond)+a_i_cond))+(cr_i_cond*((2.0*a_i_cond)+a_e_cond)))*
     5 (d_g-y_i))/(3.0*b_cond*cf_e_cond*(eulers**3)*a_e_cond)))
      
      
      R_DE_rear=2/(1+(cf_e_cond/cr_e_cond)+((((eulers**3)-1)
     1 *sqrt(pi/3.0)*((cf_e_cond*((2.0*a_e_cond)+a_i_cond))+(cf_i_cond*
     2 ((2.0*a_i_cond)+a_e_cond)))*(d_g-y_i))/(3.0*b_cond*cr_e_cond*
     3 (eulers**3)*a_e_cond))+((((eulers**3)-1)*sqrt(pi/3.0)*((cr_e_cond*
     4 ((2.0*a_e_cond)+a_i_cond))+(cr_i_cond*((2.0*a_i_cond)+a_e_cond)))*
     5 (d_g-y_i))/(3.0*b_cond*cr_e_cond*(eulers**3)*a_e_cond)))
      
      
      R_Conical_front=2/(1+(((cr_e_cond*((2.0*a_e_cond)+a_i_cond))+
     1 (cr_i_cond*((2.0*a_i_cond)+a_e_cond)))/((cf_e_cond*((2.0*a_e_cond)
     2 +a_i_cond))+(cf_i_cond*((2.0*a_i_cond)+a_e_cond))))+((3.0*b_cond
     3 *cf_e_cond*(eulers**3)*a_e_cond*sqrt(3.0/pi))/(((eulers**3)-1)*
     4 ((cf_e_cond*((2.0*a_e_cond)+a_i_cond))+(cf_i_cond*((2.0*a_i_cond)
     5 +a_e_cond)))*(d_g-y_i)))+((3.0*b_cond*cr_e_cond*(eulers**3)
     6 *a_e_cond*sqrt(3.0/pi))/(((eulers**3)-1)*((cf_e_cond*
     7 ((2.0*a_e_cond)+a_i_cond))+(cf_i_cond*((2.0*a_i_cond)+a_e_cond)))*
     8 (d_g-y_i))))
      
      
      R_Conical_rear=2/(1+(((cf_e_cond*((2.0*a_e_cond)+a_i_cond))+
     1 (cf_i_cond*((2.0*a_i_cond)+a_e_cond)))/((cr_e_cond*((2.0*a_e_cond)
     2 +a_i_cond))+(cr_i_cond*((2.0*a_i_cond)+a_e_cond))))+((3.0*b_cond
     3 *cf_e_cond*(eulers**3)*a_e_cond*sqrt(3.0/pi))/(((eulers**3)-1)*
     4 ((cr_e_cond*((2.0*a_e_cond)+a_i_cond))+(cr_i_cond*((2.0*a_i_cond)
     5 +a_e_cond)))*(d_g-y_i)))+((3.0*b_cond*cr_e_cond*(eulers**3)
     6 *a_e_cond*sqrt(3.0/pi))/(((eulers**3)-1)*((cr_e_cond*
     7 ((2.0*a_e_cond)+a_i_cond))+(cr_i_cond*((2.0*a_i_cond)+a_e_cond)))*
     8 (d_g-y_i))))
      


      Q=Voltage*Current*eff


          
      If(y.LT.y_i)then
      FLUX(1)=0
      Else
        If(yloc.GT.0)then
            If(zloc.GE.0)then !Front DE Part
            
      Wa=((6.00*Q*R_DE_front*sqrt(3.00))/(a_e_cond*b_cond
     & *cf_e_cond*pi*sqrt(pi)))
      ea=(((xloc/a_e_cond)**2)+((yloc/b_cond)**2)
     & +((zloc/cf_e_cond)**2)) 
      FLUX(1)=Wa*EXP(-3*ea)
            
            Else If(zloc.LT.0)then !Rear DE part
         
      Wb=((6.00*Q*R_DE_rear*sqrt(3.00))/(a_e_cond*b_cond
     & *cr_e_cond*pi*sqrt(pi)))
      eb=(((xloc/a_e_cond)**2)+((yloc/b_cond)**2)
     & +((zloc/cr_e_cond)**2))
         FLUX(1)=Wb*EXP(-3*eb)  
            ENDIF        
        Else If(yloc.LE.0)then
            If(zloc.GE.0)then !Front conical part
      Wc=(54.00*(eulers**3)*R_Conical_front*Q)/(pi*pi*((eulers**3)-1)
     & *(d_g-y_i)*((cf_e_cond*((2.0*a_e_cond)+a_i_cond))+(cf_i_cond*
     & ((2.0*a_i_cond)+a_e_cond))))
      ec=((zloc**2)/((cf_e_cond-((cf_e_cond-cf_i_cond)*((d_g-y)
     & /(d_g-y_i))))**2))+((xloc**2)/((a_e_cond-((a_e_cond-a_i_cond)
     & *((d_g-y)/(d_g-y_i))))**2))      
      FLUX(1)=Wc*EXP(-3*ec)      
            Else If(zloc.LT.0)then !Rear conical part
       Wd=(54.00*(eulers**3)*R_Conical_rear*Q)/(pi*pi*((eulers**3)-1)
     & *(d_g-y_i)*((cr_e_cond*((2.0*a_e_cond)+a_i_cond))+(cr_i_cond*
     & ((2.0*a_i_cond)+a_e_cond))))
      ed=((zloc**2)/((cr_e_cond-((cr_e_cond-cr_i_cond)*((d_g-y)
     & /(d_g-y_i))))**2))+((xloc**2)/((a_e_cond-((a_e_cond-a_i_cond)
     & *((d_g-y)/(d_g-y_i))))**2))      
      FLUX(1)=Wd*EXP(-3*ed)             
            
            
            
           ENDIF 
        ENDIF
      ENDIF
        
        
      IF((z.GT.340).OR.(z.LT.70))Then
      FLUX(1)=0
      ENDIF


      

      RETURN
      END
