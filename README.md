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

**ZCL_MM_QUOTA_ARRANGEMENT
class ZCL_MM_QUOTA_ARRANGEMENT definition
  public
  final
  create public .

public section.

  types:
*& Structure for Upload Quota Arrangement
    BEGIN OF ty_quota_upload,
        matnr    TYPE matnr,
        werks    TYPE equk-werks,
        bdatu_01 TYPE bdatu,
        selkz_01 TYPE selkz,
        beskz_01 TYPE beskz,
        sobin_01 TYPE sobin,
        lifnr_01 TYPE lifnr,
        bewrk_01 TYPE bewrk,
        verid_01 TYPE verid,
        quote_01 TYPE quote,
        maxmg_01 TYPE qumax,
        maxls_01 TYPE maxls,
        minls_01 TYPE minls,
      END OF ty_quota_upload .
  types:
*& Structure for Message
    BEGIN OF ty_msg,
        msgtyp TYPE bdcmsgcoll-msgtyp,
        msgv1  TYPE bdcmsgcoll-msgv1,
      END OF ty_msg .
  types:
*& Types Declarations
    tt_bdc_data TYPE TABLE OF bdcdata .
  types:
    tt_excel_data TYPE TABLE OF alsmex_tabline .
  types:
    tt_quota_upload TYPE TABLE OF ty_quota_upload WITH EMPTY KEY .
  types:
    tt_message TYPE TABLE OF ty_msg WITH EMPTY KEY .

  constants GC_A type CHAR1 value 'A' ##NO_TEXT.
  constants GC_N type CHAR1 value 'N' ##NO_TEXT.
  constants GC_E type CHAR1 value 'E' ##NO_TEXT.
  constants GC_OPEN type STRING value 'OPEN FILE' ##NO_TEXT.
  constants GC_X type BDCDATA-DYNBEGIN value 'X' ##NO_TEXT.
  constants GC_MEQ1 type CHAR5 value 'MEQ1' ##NO_TEXT.

  class-methods BDC_DYNPRO
    importing
      !P_V_PROGRAM type BDC_PROG
      !P_V_DYNPRO type BDC_DYNR
    changing
      !P_V_BDCDATA_T type TT_BDC_DATA .
  class-methods BDC_FIELD
    importing
      !P_V_FIELDNAME type FNAM_____4
      !P_V_FIELDVALUE type BDC_FVAL
    changing
      !P_V_BDCDATA_T type TT_BDC_DATA .
  class-methods EXCEL_TO_INTERNAL_TABLE
    importing
      !P_V_FILE type RLGRAP-FILENAME
    exporting
      !P_V_EXCEL_DATA type TT_EXCEL_DATA .
  class-methods CONVERT_ROW_TO_COLUMNS
    importing
      !P_V_EXCEL_DATA type TT_EXCEL_DATA
    exporting
      !P_V_QUOTA_ARRANGEMENT type TT_QUOTA_UPLOAD .
  class-methods GET_FILE
    exporting
      !P_V_FILE type RLGRAP-FILENAME .
  class-methods DISPLAY
    changing
      !P_V_MESSAGE_T type TT_MESSAGE .
  class-methods CALL_BDC_TCODE
    importing
      !P_V_FILE type RLGRAP-FILENAME
      !P_V_MODE type CTU_PARAMS-DISMODE .
  class-methods MEQ1_QUOTA_UPLOAD
    importing
      !P_V_QUOTA_ARRANGEMENT type TT_QUOTA_UPLOAD
      !P_V_MODE type CTU_PARAMS-DISMODE
    exporting
      !P_V_MESSAGE_T type TT_MESSAGE .

  method BDC_DYNPRO.
    DATA lt_bdc_data TYPE TABLE OF bdcdata.

    lt_bdc_data = VALUE #( ( dynpro   = p_v_dynpro
                             program  = p_v_program
                             dynbegin = abap_true   ) ).
    CHECK lt_bdc_data IS NOT INITIAL.
    APPEND LINES OF lt_bdc_data TO p_v_bdcdata_t.
  endmethod.

    method BDC_FIELD.
    DATA: lt_bdc_data TYPE TABLE OF bdcdata.

    lt_bdc_data = VALUE #( ( fnam = p_v_fieldname
                             dynpro = space
                             fval = p_v_fieldvalue ) ).
    CHECK lt_bdc_data IS NOT INITIAL.
    APPEND LINES OF lt_bdc_data TO p_v_bdcdata_t.
  endmethod.

    method EXCEL_TO_INTERNAL_TABLE.
***==>Call function for converting excel data to internal table
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_v_file
        i_begin_col             = 1
        i_begin_row             = 2
        i_end_col               = 100
        i_end_row               = 5000
      TABLES
        intern                  = p_v_excel_data
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CHECK p_v_excel_data IS NOT INITIAL.

    SORT p_v_excel_data BY row.
    SORT p_v_excel_data BY row col.
  endmethod.

    method CONVERT_ROW_TO_COLUMNS.

    DATA : ls_aa_data TYPE ty_quota_upload.
    FIELD-SYMBOLS <fs_excel> TYPE any.
**==>Loop internal table to workarea
    LOOP AT p_v_excel_data ASSIGNING FIELD-SYMBOL(<fs_excel_data>).

      ASSIGN COMPONENT <fs_excel_data>-col OF STRUCTURE ls_aa_data TO <fs_excel> . "#EC CI_FLDEXT_OK
      IF <fs_excel> IS ASSIGNED.
        <fs_excel> = <fs_excel_data>-value.
      ENDIF.
      AT END OF row.
        APPEND ls_aa_data TO p_v_quota_arrangement.
        CLEAR ls_aa_data.
      ENDAT.
    ENDLOOP.
  endmethod.

    method GET_FILE.
    DATA: lt_filetable TYPE filetable,
          lv_rc        TYPE i.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title = gc_open
      CHANGING
        file_table   = lt_filetable
        rc           = lv_rc.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ELSE.
      ASSIGN lt_filetable[ 1 ] TO FIELD-SYMBOL(<fs_filetable>).
      IF <fs_filetable> IS ASSIGNED.
        p_v_file = <fs_filetable>-filename.
      ENDIF.
    ENDIF.
  endmethod.

  METHOD display.
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
          lo_layout     TYPE REF TO cl_salv_layout.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
*            r_salv_table   =     DATA(lt_table)                       " Basis Class Simple ALV Tables
             r_salv_table   = lt_table
          CHANGING
            t_table        =  p_v_message_t
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
*    lt_table->display( ).
  ENDMETHOD.

  method CALL_BDC_TCODE.

*& Calling Method Excel To Internal Table
    excel_to_internal_table(
      EXPORTING   p_v_file       =   p_v_file               " Local file for upload/download
      IMPORTING   p_v_excel_data =  DATA(lt_excel_data) ).

*&Calling Method Convert row to columns
    convert_row_to_columns(
      EXPORTING p_v_excel_data        = lt_excel_data
      IMPORTING p_v_quota_arrangement = DATA(lt_upload_quota_Argmnt) ).

    CHECK lt_upload_quota_argmnt IS NOT INITIAL.
    SORT lt_upload_quota_Argmnt ASCENDING BY matnr werks.

*&Calling Method MEQ1_QUOTA_UPLOAD
    meq1_quota_upload(
      EXPORTING  p_v_quota_arrangement = lt_upload_quota_argmnt
                 p_v_mode              = p_v_mode                 " Processing mode for CALL TRANSACTION USING...
      IMPORTING  p_v_message_t         = DATA(lt_message)   ).

    EXPORT lt_message FROM lt_message TO MEMORY ID 'ZMM_QUOTA_MESG'.
**&Calling Method Display Message
*    display(
*        CHANGING  p_v_message_t = lt_message ).

  endmethod.

  METHOD meq1_quota_upload.

    DATA: lt_bdc_data TYPE TABLE OF bdcdata WITH EMPTY KEY.
    DATA: lt_meq1_msg TYPE TABLE OF bdcmsgcoll.
    DATA ls_msg TYPE ty_msg.
    DATA: lv_count TYPE numc2.
    DATA: lv_text  TYPE fnam_____4.

    DATA(lt_quota_management_temp) =  p_v_quota_arrangement[].

    DATA : lt_quota_status TYPE STANDARD TABLE OF zmm_quota_status,
           gt_meq1_msg     TYPE TABLE OF bdcmsgcoll.
*           ls_quota_status TYPE zmm_quota_status

    DELETE ADJACENT DUPLICATES FROM lt_quota_management_temp COMPARING matnr werks.

    FREE : gt_meq1_msg[], lt_quota_status[].

    LOOP AT lt_quota_management_temp ASSIGNING FIELD-SYMBOL(<fs_aa_data>).

      bdc_dynpro( EXPORTING p_v_program    = 'SAPDM06Q'
                            p_v_dynpro     = '0200'
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field(  EXPORTING p_v_fieldname  = 'BDC_CURSOR'
                            p_v_fieldvalue = 'EQUK-WERKS'
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field(  EXPORTING p_v_fieldname  = 'BDC_OKCODE'
                            p_v_fieldvalue = '/00'
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field(  EXPORTING p_v_fieldname  = 'EQUK-MATNR'
                            p_v_fieldvalue = |{ <fs_aa_data>-matnr }|
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field(  EXPORTING p_v_fieldname  = 'EQUK-WERKS'
                            p_v_fieldvalue = |{ <fs_aa_data>-werks }|
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

      bdc_dynpro( EXPORTING p_v_program    = 'SAPDM06Q'
                            p_v_dynpro     = '0205'
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field(  EXPORTING p_v_fieldname  = 'BDC_CURSOR'
                            p_v_fieldvalue = 'EQUK-BDATU(01)'
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field(  EXPORTING p_v_fieldname  = 'BDC_OKCODE'
                            p_v_fieldvalue = '/00'
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field( EXPORTING p_v_fieldname  = 'EQUK-BDATU(01)'
                           p_v_fieldvalue = |{ <fs_aa_data>-bdatu_01 }|
                 CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

      bdc_dynpro( EXPORTING p_v_program   = 'SAPDM06Q'
                           p_v_dynpro     = '0205'
                 CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field( EXPORTING p_v_fieldname  = 'BDC_CURSOR'
                           p_v_fieldvalue = 'EQUK-VDATU(01)'
                 CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field( EXPORTING p_v_fieldname  = 'BDC_OKCODE'
                           p_v_fieldvalue = '=POSI'
                 CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field( EXPORTING p_v_fieldname  = 'RM06Q-SELKZ(01)'
                           p_v_fieldvalue   = |{ <fs_aa_data>-selkz_01 }|
                 CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

      bdc_dynpro( EXPORTING p_v_program    = 'SAPDM06Q'
                            p_v_dynpro     = '0215'
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

      bdc_field( EXPORTING p_v_fieldname  = 'BDC_CURSOR'
                           p_v_fieldvalue = 'EQUP-QUOTE(01)'
                 CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      bdc_field( EXPORTING p_v_fieldname  = 'BDC_OKCODE'
                           p_v_fieldvalue = '/00'
                 CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      lv_count = 0.
      LOOP AT p_v_quota_arrangement ASSIGNING FIELD-SYMBOL(<fs_qa_data>) WHERE matnr = <fs_aa_data>-matnr AND werks = <fs_aa_data>-werks.

        lv_count = lv_count + 1.


        bdc_field( EXPORTING p_v_fieldname  = |EQUP-BESKZ({ lv_count })|
                            p_v_fieldvalue = |{ <fs_qa_data>-beskz_01 }|
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field( EXPORTING p_v_fieldname  = |RM06Q-SOBIN({ lv_count })|
                             p_v_fieldvalue = |{ <fs_qa_data>-sobin_01 }|
                   CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field( EXPORTING p_v_fieldname  = |EQUP-LIFNR({ lv_count })|
                              p_v_fieldvalue = |{ <fs_qa_data>-lifnr_01 }|
                   CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field( EXPORTING p_v_fieldname  = |EQUP-QUOTE({ lv_count })|
                             p_v_fieldvalue = |{ <fs_qa_data>-quote_01 }|
                   CHANGING   p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field( EXPORTING p_v_fieldname  = |EQUP-BEWRK({ lv_count })|
                             p_v_fieldvalue = |{ <fs_qa_data>-bewrk_01 }|
                   CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field( EXPORTING p_v_fieldname  = |EQUP-VERID({ lv_count })|
                             p_v_fieldvalue = |{ <fs_qa_data>-verid_01 }|
                   CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field(  EXPORTING p_v_fieldname  = 'BDC_OKCODE'
                              p_v_fieldvalue = '/00'
                    CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field( EXPORTING  p_v_fieldname  = 'BDC_CURSOR'
                              p_v_fieldvalue = |EQUP-MAXLS({ lv_count })|
                   CHANGING   p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field( EXPORTING  p_v_fieldname  = 'BDC_OKCODE'
                              p_v_fieldvalue = '/00'
                   CHANGING   p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field( EXPORTING  p_v_fieldname  = |EQUP-MAXMG({ lv_count })|
                              p_v_fieldvalue = |{ <fs_qa_data>-maxmg_01 }|
                   CHANGING   p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field( EXPORTING  p_v_fieldname  = |EQUP-MAXLS({ lv_count })|
                              p_v_fieldvalue = |{ <fs_qa_data>-maxls_01 }|
                    CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

        bdc_field( EXPORTING  p_v_fieldname  = |EQUP-MINLS({ lv_count })|
                              p_v_fieldvalue = |{ <fs_qa_data>-minls_01 }|
                    CHANGING  p_v_bdcdata_t  = lt_bdc_data ).
      ENDLOOP.
      bdc_dynpro( EXPORTING p_v_program    = 'SAPDM06Q'
                            p_v_dynpro     = '0215'
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

      bdc_field( EXPORTING  p_v_fieldname  = 'BDC_CURSOR'
                            p_v_fieldvalue = 'EQUP-MAXLS(01)'
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

      bdc_field(  EXPORTING p_v_fieldname  = 'BDC_OKCODE'
                            p_v_fieldvalue = '=BU'
                  CHANGING  p_v_bdcdata_t  = lt_bdc_data ).

      CASE p_v_mode.
        WHEN gc_a.
          CALL TRANSACTION gc_meq1 USING lt_bdc_data MODE gc_a  MESSAGES INTO lt_meq1_msg.
        WHEN gc_n.
          CALL TRANSACTION gc_meq1 USING lt_bdc_data MODE gc_n  MESSAGES INTO lt_meq1_msg.
        WHEN gc_e.
          CALL TRANSACTION gc_meq1 USING lt_bdc_data MODE gc_e  MESSAGES INTO lt_meq1_msg.
      ENDCASE.

      CLEAR: lt_bdc_data.

      IF sy-subrc EQ 0.
        COMMIT WORK.
        lt_quota_status = VALUE #(  BASE lt_quota_status
                                 (  mandt = sy-mandt
                                    matnr = <fs_aa_data>-matnr
                                    werks = <fs_aa_data>-werks
                                    zstatus = 'S' ) ).
      ELSE.
        lt_quota_status = VALUE #(  BASE lt_quota_status
                         (  mandt = sy-mandt
                            matnr = <fs_aa_data>-matnr
                            werks = <fs_aa_data>-werks
                            zstatus = 'F' ) ).
*      COMMIT WORK.
        IF NOT lt_meq1_msg IS INITIAL.
          gt_meq1_msg = VALUE #( BASE gt_meq1_msg
                                 FOR  ls_meq1_msg IN lt_meq1_msg
                               (  tcode  = ls_meq1_msg-tcode
                                  dyname = ls_meq1_msg-dyname
                                  dynumb = ls_meq1_msg-dynumb
                                  msgtyp = ls_meq1_msg-msgtyp
                                  msgspra = ls_meq1_msg-msgspra
                                  msgid = ls_meq1_msg-msgid
                                  msgnr = ls_meq1_msg-msgnr
                                  msgv1 = ls_meq1_msg-msgv1
                                  msgv2 = ls_meq1_msg-msgv2
                                  msgv3 = ls_meq1_msg-msgv3
                                  msgv4 = ls_meq1_msg-msgv4
                                  env   = ls_meq1_msg-env
                                  fldname = ls_meq1_msg-fldname ) ).

*          LOOP AT lt_meq1_msg ASSIGNING FIELD-SYMBOL(<fs_meq1_msg>).
*
*            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*              EXPORTING
*                msgid               = <fs_meq1_msg>-msgid
*                msgnr               = <fs_meq1_msg>-msgnr
*                msgv1               = <fs_meq1_msg>-msgv1
*                msgv2               = <fs_meq1_msg>-msgv2
*                msgv3               = <fs_meq1_msg>-msgv3
*                msgv4               = <fs_meq1_msg>-msgv4
*              IMPORTING
*                message_text_output = ls_msg-msgv1.
*
*            ls_msg-msgtyp      =    <fs_meq1_msg>-msgtyp.
*
*            APPEND ls_msg TO p_v_message_t.
*
*            CLEAR :ls_msg.
*
*          ENDLOOP.
          CLEAR: lt_meq1_msg.
        ENDIF.

      ENDIF.     "Added By Abhijit
    ENDLOOP.

    IF lt_quota_status IS NOT INITIAL.
      IF line_exists( lt_quota_status[ zstatus = 'S' ] ).
        SELECT  matnr,
                werks,
                bdatu,
                vdatu,
                qunum FROM v_wequ INTO TABLE @DATA(lt_wequ)
          FOR ALL ENTRIES IN @lt_quota_status
          WHERE matnr = @lt_quota_status-matnr AND
                werks = @lt_quota_status-werks.

        p_v_message_t = VALUE #(  BASE p_v_message_t
                                  FOR ls_quota_status   IN lt_quota_status  WHERE ( zstatus = 'S')
                                  FOR ls_wequ           IN lt_wequ          WHERE ( matnr = ls_quota_status-matnr AND werks = ls_quota_status-werks )
                               (  msgtyp = 'S'
                                  msgv1 = |{ 'Material' }| && | { <fs_aa_data>-matnr } | && |{ 'with Plant' } | &&
                                                  |{ <fs_aa_data>-werks }| && | { 'Quota Arrangement Number' } | &&
                                                  |{ ls_wequ-qunum }| && | { 'has uploaded successfully' }|
                                        ) ).
      ENDIF.

      IF line_exists( lt_quota_status[ zstatus = 'F' ] ).

        LOOP AT gt_meq1_msg ASSIGNING FIELD-SYMBOL(<fs_meq1_msg>).
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = <fs_meq1_msg>-msgid
              msgnr               = <fs_meq1_msg>-msgnr
              msgv1               = <fs_meq1_msg>-msgv1
              msgv2               = <fs_meq1_msg>-msgv2
              msgv3               = <fs_meq1_msg>-msgv3
              msgv4               = <fs_meq1_msg>-msgv4
            IMPORTING
              message_text_output = ls_msg-msgv1.

          ls_msg-msgtyp      =    <fs_meq1_msg>-msgtyp.
          APPEND ls_msg TO p_v_message_t.

          CLEAR :ls_msg.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.
