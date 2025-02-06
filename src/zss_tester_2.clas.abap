CLASS zss_tester_2 DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      if_oo_adt_classrun.

    TYPES:
      BEGIN OF post_s,
        user_id TYPE i,
        id      TYPE i,
        title   TYPE string,
        body    TYPE string,
      END OF post_s,

      post_tt TYPE TABLE OF post_s WITH EMPTY KEY,

      BEGIN OF post_without_id_s,
        user_id TYPE i,
        title   TYPE string,
        body    TYPE string,
      END OF post_without_id_s.

      TYPES: tt_str_tab TYPE STANDARD TABLE OF string WITH EMPTY KEY
           ,
           BEGIN OF ts_data,
             data TYPE string,
             tab  TYPE tt_str_tab,
           END OF ts_data.

       DATA: ls_receive  TYPE ts_data,
          lt_response TYPE ts_data.
          data : it_item1 TYPE REF TO data.

    METHODS:
      create_client
        IMPORTING url           TYPE string
        RETURNING VALUE(result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check,

      read_posts
        RETURNING VALUE(result) TYPE string.
*        RAISING   cx_static_check.



  PRIVATE SECTION.
    CONSTANTS:
      base_url     TYPE string VALUE 'https://lmiapi.estonetech.in/api/SAP_Integration/SapPurchaseOrder?Order_Id=599',
      content_type TYPE string VALUE 'Content-type',
      json_content TYPE string VALUE 'application/json; charset=UTF-8'.
ENDCLASS.



CLASS ZSS_TESTER_2 IMPLEMENTATION.


  METHOD create_client.
    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    result = cl_web_http_client_manager=>create_by_http_destination( dest ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    TRY.
        " Read
        DATA(all_posts) = read_posts(  ).


      CATCH cx_root INTO DATA(exc).
        out->write( exc->get_text(  ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD read_posts.
    " Get JSON of all posts
    DATA(url) = |{ base_url }|.
    TRY.
        data(client) = create_client( url ).
      CATCH cx_static_check.
        "handle exception
    ENDTRY.
    TRY.
        data(response) = client->execute( if_web_http_client=>get )->get_text(  ).
      CATCH cx_web_http_client_error cx_web_message_error.
        "handle exception
    ENDTRY.

*    ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
 DATA : lv_str1  TYPE string,
             lv_str2  TYPE string,
             lv_str3  TYPE string,
             lv_str4  TYPE string,
             lv_str5  TYPE string,
             lv_str6  TYPE string,
             lv_str7  TYPE string,
             lv_str8  TYPE string,
             lv_str9  TYPE string,
             lv_str10 TYPE string.
 split  response at '},' INTO  lv_str1  lv_str2 lv_str3 lv_str4 lv_str5
                                        lv_str6  lv_str7 lv_str8 lv_str9 lv_str10.
 clear:lv_str1,lv_str2,lv_str3.
 SPLIT lv_str4 AT ':"' INTO lv_str1 lv_str2.
 data(zkunnr) = lv_str2+0(10).
 clear:lv_str1 ,lv_str2 ,lv_str3, lv_str4, lv_str6,  lv_str7, lv_str8, lv_str9, lv_str10.
 split lv_str5 at '":' into lv_str1 lv_str2 lv_str3 lv_str4 lv_str6  lv_str7 lv_str8 lv_str9 lv_str10.
 clear:lv_str1 ,lv_str2 ,lv_str3, lv_str4.
 split lv_str6 at ',' into  data(Dose_Id) lv_str1.
 clear:lv_str1.
 split lv_str7 at ',' into  data(Unit_Price) lv_str1.
 clear:lv_str1.
 split lv_str8 at ',' into  data(Dose_Transport_Cost) lv_str1.
 clear:lv_str1.
 split lv_str9 at ',' into  data(Pass_Through_Cost) lv_str1.





    TRY.
        client->close(  ).
      CATCH cx_web_http_client_error.
        "handle exception
    ENDTRY.
result = response.
    " Convert JSON to post table
*    xco_cp_json=>data->from_string( response )->apply(
*      VALUE #( ( xco_cp_json=>transformation->camel_case_to_underscore ) )
*      )->write_to( REF #( result ) ).
  ENDMETHOD.
ENDCLASS.
