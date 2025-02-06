CLASS zcl_bs_demo_job_adt_exe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apj_dt_exec_object .
    INTERFACES if_apj_rt_exec_object .
    INTERFACES : if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BS_DEMO_JOB_ADT_EXE IMPLEMENTATION.


  METHOD if_apj_dt_exec_object~get_parameters.
  et_parameter_def = VALUE #(  ( selname      = 'RUN'
                                  kind         = if_apj_dt_exec_object=>parameter
                                  param_text   = 'Run'
                                  length       = 1
                                  checkbox_ind = abap_true ) ).

   et_parameter_val =  value #( sign   = 'I'
                                option = 'EQ'
                                ( selname = 'RUN' low = abap_true ) ).
  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.
  DATA : lr_out  type ref to if_oo_adt_classrun_out.
  DATA : lc_class type ref to zcl_automation.

         lc_class = new #( ).
         lc_class->if_oo_adt_classrun~main( out = lr_out ).

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

  ENDMETHOD.
ENDCLASS.
