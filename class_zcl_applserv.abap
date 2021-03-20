CLASS zcl_applserv DEFINITION PUBLIC  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF tys_file_info,
             name      TYPE pfeflnamel,
             size      TYPE pfeflsize,
             directory TYPE pfeflnamel,
           END OF tys_file_info,
           tyt_file_info TYPE STANDARD TABLE OF tys_file_info.

*    TYPES: tyt_file TYPE STANDARD TABLE OF epsfili.
    CLASS-METHODS load_xmldoc_from_applserv
      IMPORTING
        !iv_filepath   TYPE string
      RETURNING
        VALUE(rif_doc) TYPE REF TO if_ixml_document
      RAISING
        /lpp/cx_exp_withlog .

    CLASS-METHODS: log_name_to_directory IMPORTING iv_logical_filename TYPE fileintern
                                         RETURNING VALUE(rv_dir)       TYPE string.


    CLASS-METHODS: get_path IMPORTING is_file        TYPE tys_file_info
                            RETURNING VALUE(rv_path) TYPE string.

    CLASS-METHODS save_xml_doc_on_applsrv
      IMPORTING
                !iv_filepath TYPE string
                !if_xml_doc  TYPE REF TO if_ixml_document
      RETURNING VALUE(rv_rc) TYPE i.

    CLASS-METHODS: get_xml_list_from_directory
      IMPORTING iv_DIR_NAME TYPE epsdirnam
      EXPORTING et_xml      TYPE tyt_file_info.



    CLASS-METHODS create_file IMPORTING is_file TYPE tys_file_info
                                        iv_data TYPE xstring
                              RAISING   /lpp/cx_exp_withlog.


    CLASS-METHODS: get_binary_file IMPORTING is_file TYPE tys_file_info
                                   EXPORTING ev_data TYPE xstring
                                             ev_size TYPE i
                                   RAISING   /lpp/cx_exp_withlog.


    CLASS-METHODS: delete_file IMPORTING is_file        TYPE tys_file_info
                               RETURNING VALUE(rv_file) TYPE string
                               RAISING   /lpp/cx_exp_withlog.

    CLASS-METHODS: move_file IMPORTING is_src_file  TYPE tys_file_info
                                       is_dest_file TYPE tys_file_info
                             RAISING   /lpp/cx_exp_withlog.


  PROTECTED SECTION.
    CLASS-DATA: sv_file_separator TYPE char1.
    CLASS-METHODS: get_file_separator RETURNING VALUE(r_sep) TYPE char1.

ENDCLASS.



CLASS zcl_applserv IMPLEMENTATION.
  METHOD load_xmldoc_from_applserv.
*--------------------------------------
    DATA: lv_inputstring TYPE xstring.
*--------------------------------------
*-- read the XML document from a dataset into a string
    OPEN DATASET  iv_filepath  FOR INPUT IN BINARY MODE.
    IF sy-subrc NE 0.
      /lpp/cx_exp_withlog=>raise_exception(
        EXPORTING
          i_msg_class = 'ZLP_OMP'
          i_msgnr     =  '001'
          i_v1        = iv_filepath  ).
    ENDIF.
    READ DATASET  iv_filepath  INTO lv_inputstring.
    CLOSE DATASET  iv_filepath .

    DATA(ixml)  = cl_ixml=>create( ).
    DATA(lo_doc) = ixml->create_document( ).
    DATA(lo_sf) = ixml->create_stream_factory( ).
    DATA(istream) = lo_sf->create_istream_xstring( lv_inputstring ).

    DATA(iparser) = ixml->create_parser( document = lo_doc
                                         istream  = istream
                                         stream_factory = lo_sf ).
    iparser->parse( ).
    rif_doc = lo_doc.

  ENDMETHOD.

  METHOD save_xml_doc_on_applsrv.
*--------------------------------------
*--------------------------------------
    ASSERT ID zlp_omp_assert CONDITION if_xml_doc IS BOUND.
    CHECK if_xml_doc IS BOUND.
    DATA(ixml)  = cl_ixml=>create( ).

    DATA(lo_sf) = ixml->create_stream_factory( ).
    DATA(lo_stream) = lo_sf->create_ostream_uri( system_id = iv_filepath  ).

    DATA(lo_encoding) = ixml->create_encoding( character_set = 'UTF-8'
                                              byte_order = if_ixml_encoding=>co_none ).


    lo_stream->set_encoding( encoding = lo_encoding ).
    lo_stream->set_pretty_print( pretty_print = abap_true ).

* Render-Objekt
    DATA(lo_render) = ixml->create_renderer( ostream  = lo_stream
                                              document = if_xml_doc ).

* XML-String in Datei auf Applikationsserver generieren
    rv_rc = lo_render->render( ).

* Dateigröße in Bytes
    DATA(lv_size) = lo_stream->get_num_written_raw( ).

* Stream schließen
    lo_stream->close( ).

  ENDMETHOD.

  METHOD get_xml_list_from_directory.
*--------------------------------------
    DATA: lv_dir TYPE eps2filnam.
    DATA: lt_file_list  TYPE STANDARD TABLE OF  eps2fili .
*--------------------------------------
    lv_dir = iv_dir_name.

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = lv_dir
*       file_mask              = space
*      IMPORTING
*       dir_name               =
*       file_counter           =
*       error_counter          =
      TABLES
        dir_list               = lt_file_list
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.
    CHECK sy-subrc EQ 0.

    DELETE lt_file_list WHERE NOT name CP '*.xml'.

    LOOP AT lt_file_list ASSIGNING FIELD-SYMBOL(<list_item>).
      APPEND INITIAL LINE TO et_xml ASSIGNING FIELD-SYMBOL(<xml_file>).
      <xml_file>-name = <list_item>-name.
      <xml_file>-directory = lv_dir.
    ENDLOOP.

  ENDMETHOD.



  METHOD get_file_separator.
* local data----------------------------
* method body---------------------------

    IF sv_file_separator IS INITIAL.
      CALL FUNCTION 'WSAF_BUILD_SEPARATOR'
* EXPORTING
*   I_SAF_REMOTE                     =
*   I_LOCATION                       =
        IMPORTING
*         OPSYSGROUP                 =
          separator                  = sv_file_separator
*         REMOTE_NT_DRIVE            =
        EXCEPTIONS
          separator_not_maintained   = 1
          wrong_call                 = 2
          wsaf_config_not_maintained = 3
          OTHERS                     = 4.
      IF sy-subrc <> 0.
        sv_file_separator = '/'.
      ENDIF.
    ENDIF.

    r_sep = sv_file_separator.

  ENDMETHOD.

  METHOD get_binary_file.
* -------------------
* ------------------
    CLEAR: ev_size, ev_data.

    DATA(lv_path) = get_path( is_file = is_file ).

    OPEN DATASET lv_path FOR INPUT IN BINARY MODE.
    IF sy-subrc EQ 0.
      READ DATASET lv_path INTO ev_data.

      IF sy-subrc = 0.
        ev_size = xstrlen( ev_data ).
      ENDIF.
    ELSE.
*      165 Kann Datei '&1' nicht lesen
      /lpp/cx_exp_withlog=>raise_exception(
        EXPORTING
          i_msg_class = 'ZLP_OMP'
          i_msgnr     = '165'
          i_v1        = lv_path ).

    ENDIF.
    CLOSE DATASET lv_path.
  ENDMETHOD.

  METHOD get_path.
* local data-------------------
    DATA:
      l_sep TYPE c,
      l_len TYPE i.
* method body------------------

* Separator at directory end?
    l_len = strlen( is_file-directory ).
    SUBTRACT 1 FROM l_len.
    IF  is_file-directory+l_len(1) EQ '/' OR
        is_file-directory+l_len(1) EQ '\'.

      CONCATENATE
         is_file-directory
         is_file-name
         INTO
         rv_path.
      RETURN.
    ENDIF.

* No separator at directory end
    l_sep = get_file_separator( ).

    CONCATENATE
       is_file-directory
       l_sep
       is_file-name
       INTO
       rv_path.
  ENDMETHOD.

  METHOD create_file.
*--------------------------------------
*--------------------------------------
    DATA(lv_file) = get_path( is_file = is_file ).
    OPEN DATASET lv_file FOR OUTPUT IN BINARY MODE.
    IF sy-subrc = 0.

      TRANSFER iv_data TO lv_file.

      CLOSE DATASET lv_file.
    ELSE.
*      166   Kann in Datei '&1' nicht schreiben
      /lpp/cx_exp_withlog=>raise_exception(
                    i_msg_class = 'ZLP_OMP'
                    i_msgnr     = '166'
                    i_v1 = lv_file ).

    ENDIF.

  ENDMETHOD.

  METHOD delete_file.
* local data-------------------
* method body------------------

    rv_file = get_path( is_file = is_file ).

    DELETE DATASET rv_file.
    IF sy-subrc NE 0.

*      167   Kann in Datei '&1' nicht löschen
      /lpp/cx_exp_withlog=>raise_exception( i_msg_class = 'ZLP_OMP'
                                            i_msgnr     = '167'
                                            i_v1 = rv_file ).
    ENDIF.

  ENDMETHOD.

  METHOD move_file.
* local data--------------------
    DATA:
      lv_src_data TYPE xstring,
      lv_size     TYPE i,
      ls_dest     TYPE tys_file_info.
* method body-------------------

    IF is_src_file EQ is_dest_file.
      RETURN.
    ENDIF.

* read source file
    get_binary_file(
      EXPORTING
        is_file = is_src_file
      IMPORTING
        ev_data = lv_src_data
        ev_size = lv_size ).

* Write destination file
    ls_dest = is_dest_file.
    ls_dest-size = lv_size.
    create_file( is_file = ls_dest
                 iv_data = lv_src_data ).
* Delete soure file
    delete_file( is_file = is_src_file ).

  ENDMETHOD.

  METHOD log_name_to_directory.
*--------------------------------------
    DATA: lv_logical_path  TYPE pathintern.
    DATA: lv_file_name_with_path TYPE string.
    DATA: lv_pathname TYPE rstxtlg,
          lv_filename TYPE rsawbnobjnm.
*--------------------------------------
    lv_logical_path = iv_logical_filename.
    CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
      EXPORTING
        logical_path               = lv_logical_path
        operating_system           = sy-opsys
        file_name                  = 'dummy'
      IMPORTING
        file_name_with_path        = lv_file_name_with_path
      EXCEPTIONS
        path_not_found             = 1
        missing_parameter          = 2
        operating_system_not_found = 3
        file_system_not_found      = 4
        OTHERS                     = 5.
    ASSERT ID zlp_omp_assert CONDITION sy-subrc EQ 0.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'RSDS_SPLIT_PATH_TO_FILENAME'
      EXPORTING
        i_filepath = CONV rsfilenm( lv_file_name_with_path )
      IMPORTING
        e_pathname = lv_pathname
        e_filename = lv_filename.

    rv_dir = lv_pathname.

  ENDMETHOD.

ENDCLASS.