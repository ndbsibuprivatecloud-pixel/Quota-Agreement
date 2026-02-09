# Quota-Agreement
Quota Agreement Upload Program Creation

*&---------------------------------------------------------------------*
*& Report Z_MM_UPLOAD_QUOTA_ARGMT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_mm_upload_quota_argmt.

TYPES :
*& Structure for Message
  BEGIN OF ty_message,
    msgtyp TYPE bdcmsgcoll-msgtyp,
    msgv1  TYPE bdcmsgcoll-msgv1,
  END OF ty_message .

TYPES: BEGIN OF ty_alcolumn,
         lo_alv         TYPE REF TO cl_salv_table,
         im_column_name TYPE lvc_fname,
         im_longtext    TYPE scrtext_l,
       END OF ty_alcolumn,
       ty_t_alcolumn TYPE TABLE OF ty_alcolumn WITH EMPTY KEY.

DATA(go_text) = NEW zcl_rsb_utilities( ).

DATA: lr_column     TYPE REF TO cl_salv_column_table,
      lt_alcolumn   TYPE TABLE OF ty_alcolumn,

**==>declaratio of salv events
      lt_table      TYPE REF TO cl_salv_table,
      lo_events     TYPE REF TO cl_salv_events_table,
      lo_columns    TYPE REF TO cl_salv_columns_table,
      lo_alv_header TYPE REF TO cl_salv_table,
      lo_column     TYPE REF TO cl_salv_column,
      go_column     TYPE REF TO cl_salv_column_list,
      go_columns    TYPE REF TO cl_salv_columns_table,
      ls_key        TYPE salv_s_layout_key,
      lo_layout     TYPE REF TO cl_salv_layout,

      lt_message  TYPE STANDARD TABLE OF ty_message.

*TYPES : tt_message  TYPE TABLE OF ty_message WITH EMPTY KEY.

***==>Selection screen declaring parameters
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_file TYPE rlgrap-filename  OBLIGATORY. " File Name
  PARAMETERS: p_mode LIKE ctu_params-dismode DEFAULT 'N'.

SELECTION-SCREEN : END OF BLOCK b1.

**==>For Value Request
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

*& Calling Method for File
  zcl_mm_quota_arrangement=>get_file(
             IMPORTING  p_v_file = p_file     ).  " Local file for upload/download ).
*& Start Of Selection
START-OF-SELECTION.
*&Calling method For Bdc
  zcl_mm_quota_arrangement=>call_bdc_tcode(
    p_v_file = p_file                 " Local file for upload/download
    p_v_mode = p_mode               " Processing mode for CALL TRANSACTION USING...
    ).

  TRY.
      IMPORT lt_message TO lt_message FROM  MEMORY ID 'ZMM_QUOTA_MESG'.
    CATCH cx_sy_import_mismatch_error.
  ENDTRY.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
*            r_salv_table   =     DATA(lt_table)                       " Basis Class Simple ALV Tables
           r_salv_table   = lt_table
        CHANGING
          t_table        =  lt_message
      ).

      TRY.
          lt_table->set_screen_status(
            report        = sy-repid
            pfstatus      = 'ZSTATUS2'
            set_functions = cl_salv_table=>c_functions_all ).
        CATCH cx_salv_data_error.
        CATCH cx_salv_object_not_found.
      ENDTRY.
      lo_events = lt_table->get_event( ).

**==>Get Column Names for ALV Display
      lo_columns = lt_table->get_columns( ).

      CLEAR lt_alcolumn[].

**==> Layout Code
      ls_key-report = sy-repid.
      lo_layout = lt_table->get_layout( ).
      lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
      lo_layout->set_key( ls_key ).

      lt_alcolumn = VALUE ty_t_alcolumn( (  im_column_name = 'MSGTYP'  im_longtext = 'BDC Message Type' )
                                         (  im_column_name = 'MSGV1'  im_longtext = 'Description' )
                                        ) .

      LOOP AT lt_alcolumn ASSIGNING FIELD-SYMBOL(<fs_alcoloumn>).
        CALL METHOD go_text->salv_text
          EXPORTING
            lo_salv_table   = lt_table
            im_coloumn_name = <fs_alcoloumn>-im_column_name
            im_longtext     = <fs_alcoloumn>-im_longtext.
      ENDLOOP.

      lt_table->display( ).

    CATCH cx_salv_not_found.
    CATCH cx_salv_msg.
  ENDTRY.
