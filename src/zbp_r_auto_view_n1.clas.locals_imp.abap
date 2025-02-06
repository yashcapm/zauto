CLASS lhc_zr_auto_view DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zr_auto_view RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zr_auto_view RESULT result.

    METHODS autopost FOR MODIFY
      IMPORTING keys FOR ACTION zr_auto_view~autopost RESULT result.

    METHODS getdata FOR MODIFY
      IMPORTING keys FOR ACTION zr_auto_view~getdata RESULT result.

    METHODS grnpost FOR MODIFY
      IMPORTING keys FOR ACTION zr_auto_view~grnpost RESULT result.

    METHODS invpost FOR MODIFY
      IMPORTING keys FOR ACTION zr_auto_view~invpost RESULT result.

ENDCLASS.

CLASS lhc_zr_auto_view IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD autopost.
  ENDMETHOD.

  METHOD getdata.
  ENDMETHOD.

  METHOD grnpost.
  ENDMETHOD.

  METHOD invpost.
    TYPES:BEGIN OF ty_so,
            plant TYPE werks_d,
          END OF ty_so.

    DATA :it_so TYPE STANDARD TABLE OF ty_so,
          wa_so TYPE ty_so.


    READ ENTITIES OF zr_auto_view IN LOCAL MODE
   ENTITY zr_auto_view ALL FIELDS WITH CORRESPONDING #( keys ) RESULT FINAL(data_read).

    LOOP AT data_read INTO DATA(w_final).
      wa_so-plant = w_final-plant.
      COLLECT wa_so INTO it_so.
      CLEAR:wa_so,w_final.
    ENDLOOP.

    SELECT * FROM zc_invnew
    FOR ALL ENTRIES IN @data_read
    WHERE kunnr = @data_read-soldtoparty
    INTO TABLE @DATA(it_main).

    DATA(it_final) = data_read[].

    LOOP AT it_main ASSIGNING FIELD-SYMBOL(<fs_main>).
      IF <fs_main>-status = 'X'.
        IF it_final[] IS NOT INITIAL.
          SELECT a~salesorder AS sddocument,
                 b~plant

              FROM i_salesorder AS a
              LEFT OUTER JOIN i_salesorderitem AS b ON a~salesorder = b~salesorder
              FOR ALL ENTRIES IN @it_final
              WHERE a~soldtoparty = @<fs_main>-kunnr             "in "('0000000163','0000000164')" a~OverallSDProcessStatus <> 'C'
              AND   a~overallsdprocessstatus <> 'C'
              AND   a~salesorder = @it_final-salesorder
              INTO TABLE @DATA(it_temp).
          IF it_temp[] IS NOT INITIAL.
            LOOP AT it_so INTO wa_so.
              CLEAR:it_final[],w_final.
              LOOP AT it_temp INTO DATA(wa_temp) WHERE plant = wa_so-plant.
                MOVE-CORRESPONDING wa_temp TO w_final.
                APPEND w_final TO it_final.
                CLEAR:w_final,wa_temp.
              ENDLOOP.


              IF it_final[] IS NOT INITIAL.
                MODIFY ENTITIES OF i_billingdocumenttp
              ENTITY billingdocument
              EXECUTE createfromsddocument AUTO FILL CID
              WITH
              VALUE  #(
              ( %param = VALUE #( _reference = VALUE #( FOR ls_final IN it_final (
                                                     sddocument = ls_final-salesorder"'0000000161'
                                 %control = VALUE #( sddocument = if_abap_behv=>mk-on ) )
               )

              %control = VALUE #( _reference = if_abap_behv=>mk-on ) ) ) )


              RESULT DATA(lt_result_modify)
              FAILED DATA(ls_failed_modify)
              REPORTED DATA(ls_reported_modify).

                IF lt_result_modify[] IS NOT INITIAL.

                ENDIF.
              ENDIF.

            ENDLOOP.
          ENDIF.
        ENDIF.
      ELSE.
        SELECT a~salesorder AS sddocument,
                b~plant

             FROM i_salesorder AS a
             LEFT OUTER JOIN i_salesorderitem AS b ON a~salesorder = b~salesorder
             FOR ALL ENTRIES IN @it_final
             WHERE a~soldtoparty = @<fs_main>-kunnr             "in "('0000000163','0000000164')" a~OverallSDProcessStatus <> 'C'
             AND   a~overallsdprocessstatus <> 'C'
             AND   a~salesorder = @it_final-salesorder
             INTO TABLE @it_temp.




        IF it_temp[] IS NOT INITIAL.
          LOOP AT it_temp INTO DATA(wa_final).
            MODIFY ENTITIES OF i_billingdocumenttp
            ENTITY billingdocument
            EXECUTE createfromsddocument AUTO FILL CID
            WITH
            VALUE  #(
            ( %param = VALUE #( _reference = VALUE #( (
                                                   sddocument = wa_final-sddocument"'0000000161'
                               %control = VALUE #( sddocument = if_abap_behv=>mk-on ) )
             )

            %control = VALUE #( _reference = if_abap_behv=>mk-on ) ) ) )


            RESULT lt_result_modify
            FAILED ls_failed_modify
            REPORTED ls_reported_modify.

            IF lt_result_modify[] IS NOT INITIAL.

            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDIF.

*    clear: it_inv[],it_inv.
    ENDLOOP.








  ENDMETHOD.

ENDCLASS.

CLASS lsc_zr_auto_view DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zr_auto_view IMPLEMENTATION.

  METHOD save_modified.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
