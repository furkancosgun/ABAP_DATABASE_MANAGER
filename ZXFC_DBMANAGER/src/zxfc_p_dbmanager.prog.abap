*&---------------------------------------------------------------------*
*& Report ZXFC_P_DBMANAGER
*&---------------------------------------------------------------------*
*& DEVELOPED BY FURKAN COSGUN
*&---------------------------------------------------------------------*
REPORT sy-repid.

PARAMETERS:p_conn TYPE dbcon-con_name DEFAULT 'DEFAULT'.

CLASS:lcl_model      DEFINITION DEFERRED.
CLASS:lcl_view       DEFINITION DEFERRED.
CLASS:lcl_controller DEFINITION DEFERRED.

DATA:_controller TYPE REF TO lcl_controller.
*----------------------------------------------------------------------*
* CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION.
  PUBLIC SECTION.
    METHODS:constructor IMPORTING  VALUE(i_dbname) TYPE dbcon_name
                        EXCEPTIONS no_connection.

    METHODS:get_data    IMPORTING  VALUE(i_query)  TYPE string
                        RETURNING  VALUE(r_result) TYPE REF TO data
                        EXCEPTIONS query_error
                                   no_result.

  PRIVATE SECTION.
    DATA :mo_con_ref    TYPE REF TO cl_sql_connection.
    DATA :mo_state   TYPE REF TO cl_sql_statement.
    DATA :mo_table_descr  TYPE REF TO cl_abap_tabledescr.
    DATA :mo_struct_descr TYPE REF TO cl_abap_structdescr.
    DATA :mt_fcat TYPE lvc_t_fcat.
    DATA :mt_meta TYPE adbc_rs_metadata_descr_tab.
    DATA :mo_result  TYPE REF TO cl_sql_result_set.
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION .
  METHOD:constructor.
    TRY .
        mo_con_ref = cl_sql_connection=>get_connection( i_dbname ).
      CATCH cx_root INTO DATA(e).
        MESSAGE e->get_text( ) TYPE 'E' RAISING no_connection.
    ENDTRY.
  ENDMETHOD.
  METHOD:get_data.
    CLEAR:mt_fcat,mo_state,mo_result,mt_meta,r_result.
    TRY .
        mo_state = mo_con_ref->create_statement( ).
        mo_result = mo_state->execute_query( i_query ).
        mt_meta = mo_result->get_metadata( ).

        LOOP AT mt_meta INTO DATA(meta).
          APPEND INITIAL LINE TO mt_fcat ASSIGNING FIELD-SYMBOL(<fs_line>).
          IF meta-column_name IS INITIAL.
            meta-column_name = TEXT-t01.
          ENDIF.
          <fs_line>-scrtext_s =
          <fs_line>-scrtext_m =
          <fs_line>-scrtext_l =
          <fs_line>-coltext   =
          <fs_line>-reptext   =
          <fs_line>-fieldname = meta-column_name.
          <fs_line>-datatype  = meta-data_type.
          <fs_line>-decimals  = meta-decimals.
          <fs_line>-intlen    = meta-length.
        ENDLOOP.
        IF sy-subrc NE 0.
          mo_result->close( ).
          MESSAGE TEXT-m02 TYPE 'E' RAISING no_result.
        ENDIF.

        cl_alv_table_create=>create_dynamic_table(
          EXPORTING
            it_fieldcatalog           = mt_fcat
          IMPORTING
            ep_table                  = r_result
          EXCEPTIONS
            generate_subpool_dir_full = 1
            OTHERS                    = 2 ).

        mo_result->set_param_table( r_result ).
        mo_result->next_package( ).
        mo_result->close( ).

      CATCH cx_root INTO DATA(e).
        MESSAGE e->get_text( ) TYPE 'E' RAISING query_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: prepare_view     EXCEPTIONS contains_error.
    METHODS: display_result   IMPORTING  VALUE(i_result) TYPE REF TO data
                              EXCEPTIONS contains_error.
    METHODS: get_editor_query RETURNING  VALUE(rv_query) TYPE string
                              EXCEPTIONS no_query.
  PRIVATE SECTION.
    DATA: mo_splitter_row     TYPE REF TO cl_gui_splitter_container,
          mo_container        TYPE REF TO cl_gui_custom_container,
          mo_container_top    TYPE REF TO cl_gui_container,
          mo_container_bottom TYPE REF TO cl_gui_container,
          mo_editor           TYPE REF TO cl_gui_abapedit,
          mo_alv              TYPE REF TO cl_salv_table.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION .
  METHOD prepare_view.
    "Split 2 Row
    CREATE OBJECT mo_splitter_row
      EXPORTING
        parent                  = cl_gui_container=>screen0
        no_autodef_progid_dynnr = abap_true
        rows                    = 2
        columns                 = 1.
    IF mo_splitter_row IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.


    "Get 1 Row For Top
    mo_container_top = mo_splitter_row->get_container(
                            row    = 1
                            column = 1
                          ).
    IF mo_container_top IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

    "Get 1 Row For Bottom
    mo_container_bottom = mo_splitter_row->get_container(
                            row    = 2
                            column = 1
                          ).
    IF mo_container_bottom IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

    "Create Editor
    CREATE OBJECT mo_editor
      EXPORTING
        parent = mo_container_top.
    IF mo_editor IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

  ENDMETHOD.
  METHOD display_result.
    ASSIGN i_result->* TO FIELD-SYMBOL(<fs_outtab>).
    IF <fs_outtab> IS INITIAL.
      MESSAGE TEXT-m01 TYPE 'E' RAISING contains_error.
    ENDIF.

    IF mo_alv IS INITIAL.
      cl_salv_table=>factory(
           EXPORTING
             r_container    = mo_container_bottom
           IMPORTING
             r_salv_table   = mo_alv
           CHANGING
             t_table        = <fs_outtab>
         ).
    ENDIF.

    mo_alv->set_data( CHANGING t_table = <fs_outtab> ).
    mo_alv->get_functions( )->set_all( abap_true ).
    mo_alv->get_columns( )->set_optimize( abap_true ).

    "IF COLUMN NAMES NOT SHOWING
    LOOP AT mo_alv->get_columns( )->get( ) INTO DATA(col).
      col-r_column->set_long_text( CONV #( col-columnname ) ).
    ENDLOOP.

    mo_alv->display( ).

  ENDMETHOD.
  METHOD:get_editor_query.
    DATA : lt_text_table TYPE TABLE OF string WITH EMPTY KEY.
    DATA : lv_line_size  TYPE i.
    CLEAR: lt_text_table.

    mo_editor->get_selected_text_as_table(
      IMPORTING
        table    = lt_text_table
      EXCEPTIONS
        error_dp = 1
        OTHERS   = 2
    ).

    IF lt_text_table IS INITIAL.
      mo_editor->get_line_count(
        IMPORTING
          lines                  =  lv_line_size    " total number of lines displayed by control.
        EXCEPTIONS
          error_cntl_call_method = 1                " Error while retrieving the total number of lines displayed
          OTHERS                 = 2
      ).
      mo_editor->select_lines(
        EXPORTING
          from_line              = 1                  " from line
          to_line                = lv_line_size                 " to line
        EXCEPTIONS
          error_cntl_call_method = 1                " Error while selecting lines within SourceEdit control!
          OTHERS                 = 2
      ).
      mo_editor->get_selected_text_as_table(
        IMPORTING
          table    = lt_text_table
        EXCEPTIONS
          error_dp = 1
          OTHERS   = 2
      ).
    ENDIF.

    LOOP AT lt_text_table INTO DATA(lv_text).
      CONCATENATE rv_query lv_text INTO rv_query SEPARATED BY space.
    ENDLOOP.

    CONDENSE rv_query.

    IF rv_query IS INITIAL.
      MESSAGE TEXT-m03 TYPE 'E' RAISING no_query.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_controller  DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:constructor.
    METHODS:display_view.
    METHODS:execute_query.

  PRIVATE SECTION.
    DATA:_view  TYPE REF TO lcl_view.
    DATA:_model TYPE REF TO lcl_model.
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_controller  IMPLEMENTATION.
  METHOD:constructor.
    _view  = NEW lcl_view( ).
    CREATE OBJECT _model
      EXPORTING
        i_dbname      = p_conn
      EXCEPTIONS
        no_connection = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD:display_view.
    _view->prepare_view(
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL SCREEN 0100.
  ENDMETHOD.
  METHOD:execute_query.
    _view->get_editor_query(
      RECEIVING
        rv_query = DATA(lv_query)
      EXCEPTIONS
        no_query = 1
        OTHERS   = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    _model->get_data(
      EXPORTING
        i_query     = lv_query
      RECEIVING
        r_result    = DATA(lr_result)
      EXCEPTIONS
        query_error = 1
        no_result   = 2
        OTHERS      = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    _view->display_result(
      EXPORTING
        i_result       = lr_result
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  _controller = NEW lcl_controller( ).

END-OF-SELECTION.
  _controller->display_view( ).

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.
  IF sy-ucomm EQ 'BACK'.
    SET SCREEN 0.
  ELSEIF sy-ucomm EQ 'EXECUTE'.
    _controller->execute_query( ).
  ENDIF.
ENDMODULE.
