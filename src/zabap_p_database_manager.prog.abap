*&---------------------------------------------------------------------*
*& Report zabap_p_database_manager
*&---------------------------------------------------------------------*
*& DEVELOPED BY FURKAN COSGUN
*&---------------------------------------------------------------------*
REPORT zabap_p_database_manager.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t_title.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) lbl_conn.
PARAMETERS:p_conn TYPE dbcon-con_name DEFAULT cl_sql_connection=>c_default_connection.
SELECTION-SCREEN:PUSHBUTTON 55(15) btn_conn USER-COMMAND conn.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN:PUSHBUTTON 55(15) btn_exec USER-COMMAND exec.
SELECTION-SCREEN END OF BLOCK b1.


CLASS:lcl_model      DEFINITION DEFERRED.
CLASS:lcl_view       DEFINITION DEFERRED.
CLASS:lcl_controller DEFINITION DEFERRED.

DATA:_controller TYPE REF TO lcl_controller.

*----------------------------------------------------------------------*
* CLASS lcl_common DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_common DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_messages,
                 result_not_found TYPE string VALUE 'Query executed successfully, but no results were found.',
                 query_not_found  TYPE string VALUE 'No query was provided.',
               END OF mc_messages,
               BEGIN OF mc_texts,
                 btn_exec_lbl TYPE string VALUE '@15@ Execute',
                 btn_conn_lbl TYPE string VALUE '@GB@ Connect',
                 lbl_conn     TYPE string VALUE 'Connection Name',
                 app_title    TYPE string VALUE 'ABAP Database Manager',
               END OF mc_texts,
               BEGIN OF mc_functions,
                 connect TYPE sy-ucomm VALUE 'CONN',
                 execute TYPE sy-ucomm VALUE 'EXEC',
               END OF mc_functions,
               mc_default_col_prefix TYPE string VALUE 'COLUMN_'.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          VALUE(ip_dbcon_name) TYPE dbcon_name
        EXCEPTIONS
          connection_failed.
    METHODS:
      execute_query
        IMPORTING
          VALUE(ip_query) TYPE string
        RETURNING
          VALUE(r_result) TYPE REF TO data
        EXCEPTIONS
          query_failed
          result_not_found.


  PRIVATE SECTION.
    DATA:
      mo_connection_ref TYPE REF TO cl_sql_connection.
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor.

    METHODS:
      display_result
        IMPORTING
          VALUE(ir_result) TYPE REF TO data.

    METHODS:
      get_query_from_editor
        RETURNING
          VALUE(r_query) TYPE string
        EXCEPTIONS
          query_not_found .
  PRIVATE SECTION.
    DATA:
      mo_splitter_row     TYPE REF TO cl_gui_splitter_container,
      mo_container_top    TYPE REF TO cl_gui_container,
      mo_container_bottom TYPE REF TO cl_gui_container,
      mo_dock             TYPE REF TO cl_gui_docking_container,
      mo_editor           TYPE REF TO cl_gui_abapedit,
      mo_alv              TYPE REF TO cl_salv_table.

ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_controller  DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      initialization,
      on_event
        IMPORTING
          ip_event TYPE sy-ucomm.

  PRIVATE SECTION.
    METHODS:
      on_connect,
      on_exec.

    DATA:
      _view  TYPE REF TO lcl_view,
      _model TYPE REF TO lcl_model.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION .
  METHOD:constructor.
    TRY .
        mo_connection_ref = cl_sql_connection=>get_connection( ip_dbcon_name ).
      CATCH cx_root INTO DATA(e).
        MESSAGE e->get_text( ) TYPE 'E' RAISING connection_failed.
    ENDTRY.
  ENDMETHOD.
  METHOD:execute_query.
    TRY .
        DATA(lo_result) = mo_connection_ref->create_statement( )->execute_query(
          EXPORTING
            statement   = ip_query
        ).

        DATA(lt_metadata) = lo_result->get_metadata( ).
        IF lt_metadata IS INITIAL.
          lo_result->close( ).
          MESSAGE lcl_common=>mc_messages-result_not_found TYPE 'E' RAISING result_not_found.
        ENDIF.

        LOOP AT lt_metadata REFERENCE INTO DATA(lr_metadata).
          IF lr_metadata->column_name IS INITIAL.
            lr_metadata->column_name = |{ lcl_common=>mc_default_col_prefix }{ sy-tabix }|.
          ENDIF.
          CASE lr_metadata->data_type.
            WHEN cl_sql_result_set=>c_md_type_p.
              IF lr_metadata->length GT cl_abap_elemdescr=>type_p_max_length.
                lr_metadata->length = cl_abap_elemdescr=>type_p_max_length.
              ENDIF.
            WHEN cl_sql_result_set=>c_md_type_c.
              IF lr_metadata->length GT cl_abap_elemdescr=>type_c_max_length.
                lr_metadata->length = cl_abap_elemdescr=>type_c_max_length.
              ENDIF.
            WHEN cl_sql_result_set=>c_md_type_x.
              IF lr_metadata->length GT cl_abap_elemdescr=>type_x_max_length.
                lr_metadata->length = cl_abap_elemdescr=>type_x_max_length.
              ENDIF.
          ENDCASE.
        ENDLOOP.

        DATA(lo_tabletype) = cl_abap_tabledescr=>create(
            p_line_type  = CAST cl_abap_structdescr(
                cl_abap_structdescr=>describe_by_data_ref(
                    p_data_ref = lo_result->get_struct_ref(
                        md_tab = lt_metadata
                    )
                )
            )
        ).

        CREATE DATA r_result TYPE HANDLE lo_tabletype.

        lo_result->set_param_table( r_result ).
        lo_result->next_package( ).
        lo_result->close( ).
      CATCH cx_root INTO DATA(e).
        MESSAGE e->get_text( ) TYPE 'E' RAISING query_failed.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION .
  METHOD constructor.
    CREATE OBJECT mo_dock
      EXPORTING
        repid = sy-repid
        dynnr = '1000'
        side  = cl_gui_docking_container=>dock_at_bottom
        ratio = 85.

    CREATE OBJECT mo_splitter_row
      EXPORTING
        parent                  = mo_dock
        no_autodef_progid_dynnr = abap_true
        rows                    = 2
        columns                 = 1.

    mo_container_top = mo_splitter_row->get_container( row = 1 column = 1 ).
    mo_container_bottom = mo_splitter_row->get_container( row = 2 column = 1 ).

    CREATE OBJECT mo_editor
      EXPORTING
        parent = mo_container_top.
  ENDMETHOD.
  METHOD display_result.
    ASSIGN ir_result->* TO FIELD-SYMBOL(<fs_outtab>).
    IF <fs_outtab> IS INITIAL.
      MESSAGE lcl_common=>mc_messages-result_not_found TYPE 'E'.
    ENDIF.

    IF mo_alv IS NOT BOUND.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = mo_container_bottom
        IMPORTING
          r_salv_table   = mo_alv
        CHANGING
          t_table        = <fs_outtab>
      ).
      mo_alv->get_functions( )->set_all( ).
      mo_alv->get_columns( )->set_optimize( ).
    ELSE.
      mo_alv->set_data( CHANGING t_table = <fs_outtab> ).
    ENDIF.

    LOOP AT mo_alv->get_columns( )->get( ) INTO DATA(col).
      col-r_column->set_long_text( CONV #( col-columnname ) ).
    ENDLOOP.

    mo_alv->display( ).
  ENDMETHOD.
  METHOD:get_query_from_editor.
    DATA:
      lt_text_table TYPE string_table,
      lv_line_size  TYPE i.

    mo_editor->get_selected_text_as_table(
      IMPORTING
        table    = lt_text_table
    ).

    IF lt_text_table IS INITIAL.
      mo_editor->get_line_count( IMPORTING lines = lv_line_size ).
      mo_editor->select_lines( from_line = 1 to_line = lv_line_size ).
      mo_editor->get_selected_text_as_table( IMPORTING table = lt_text_table ).
    ENDIF.

    CONCATENATE LINES OF lt_text_table INTO r_query SEPARATED BY space.

    IF r_query IS INITIAL.
      MESSAGE lcl_common=>mc_messages-query_not_found TYPE 'E' RAISING query_not_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_controller  IMPLEMENTATION.
  METHOD:initialization.
    DATA lt_exclude TYPE TABLE OF syucomm.

    btn_exec = lcl_common=>mc_texts-btn_exec_lbl.
    btn_conn = lcl_common=>mc_texts-btn_conn_lbl.
    lbl_conn = lcl_common=>mc_texts-lbl_conn.
    t_title  = lcl_common=>mc_texts-app_title.

    lt_exclude = VALUE #( ( 'ONLI' ) ( 'SJOB' ) ( 'PRIN' ) ).

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = sy-pfkey
        p_program = sy-repid
      TABLES
        p_exclude = lt_exclude.

  ENDMETHOD.
  METHOD:on_event.
    CASE ip_event.
      WHEN lcl_common=>mc_functions-connect.
        me->on_connect( ).
      WHEN lcl_common=>mc_functions-execute.
        me->on_exec( ).
    ENDCASE.
  ENDMETHOD.
  METHOD:on_connect.
    CREATE OBJECT _model
      EXPORTING
        ip_dbcon_name = p_conn.

    CHECK _view IS NOT BOUND.

    CREATE OBJECT _view.
  ENDMETHOD.
  METHOD:on_exec.
    CHECK _view IS BOUND .
    _view->display_result(
        _model->execute_query(
            _view->get_query_from_editor( )
        )
    ).
  ENDMETHOD.
ENDCLASS.

LOAD-OF-PROGRAM.
  _controller = NEW lcl_controller( ).

INITIALIZATION.
  _controller->initialization( ).

AT SELECTION-SCREEN.
  _controller->on_event( sy-ucomm ).
