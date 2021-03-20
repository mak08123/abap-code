CLASS zcl_doc_class_data DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    TYPES:
      tyt_doc_files TYPE STANDARD TABLE OF bapi_doc_files2 .
    TYPES:
      tyt_doc_bom_item TYPE STANDARD TABLE OF dmu_document .
    TYPES:
      tyt_matnr TYPE STANDARD TABLE OF matnr .

    CLASS-METHODS find_zaz_doc_by_fauf
      IMPORTING
        !iv_fauf        TYPE aufnr
      RETURNING
        VALUE(ro_class) TYPE REF TO zcl_doc_class_data .

    CLASS-METHODS find_znp_doc_by_fauf
      IMPORTING
        !iv_fauf        TYPE aufnr
      RETURNING
        VALUE(ro_class) TYPE REF TO zcl_doc_class_data .

    CLASS-METHODS find_zpv_doc_by_fauf
      IMPORTING
        !iv_fauf        TYPE aufnr
      RETURNING
        VALUE(ro_class) TYPE REF TO zcl_doc_class_data .
    CLASS-METHODS find_zdb_doc_by_fauf
      IMPORTING
        !iv_fauf        TYPE aufnr
      RETURNING
        VALUE(ro_class) TYPE REF TO zcl_doc_class_data .
    CLASS-METHODS find_zdr_doc_by_fauf
      IMPORTING
        !iv_fauf        TYPE aufnr
      RETURNING
        VALUE(ro_class) TYPE REF TO zcl_doc_class_data .
    METHODS constructor
      IMPORTING
        !is_doc_key TYPE bapi_doc_keys
        !iv_class   TYPE klasse_d
      RAISING
        /lpp/cx_exp_withlog .
    METHODS get_mandatory_char_atwrt
      IMPORTING
        !iv_atnam       TYPE atnam
      RETURNING
        VALUE(rv_atwrt) TYPE atwrt
      RAISING
        /lpp/cx_exp_withlog .
    METHODS get_char_atwrt
      IMPORTING
        !iv_atnam       TYPE atnam
      RETURNING
        VALUE(rv_atwrt) TYPE atwrt .
    METHODS get_mandatory_num_atwrt
      IMPORTING
        !iv_atnam     TYPE atnam
      RETURNING
        VALUE(rv_val) TYPE i
      RAISING
        /lpp/cx_exp_withlog .
    METHODS get_num_attr_integer
      IMPORTING
        !iv_atnam     TYPE atnam
      RETURNING
        VALUE(rv_val) TYPE i .
    METHODS get_mandatory_char_attr
      IMPORTING
        !iv_atnam      TYPE atnam
      RETURNING
        VALUE(rs_attr) TYPE bapi1003_alloc_values_char
      RAISING
        /lpp/cx_exp_withlog .
    METHODS get_mandatory_num_attr
      IMPORTING
        !iv_atnam     TYPE atnam
      RETURNING
        VALUE(rs_val) TYPE bapi1003_alloc_values_num
      RAISING
        /lpp/cx_exp_withlog .
    CLASS-METHODS get_class
      IMPORTING
        !is_doc_key     TYPE bapi_doc_keys
      RETURNING
        VALUE(ro_class) TYPE REF TO zcl_doc_class_data .
    METHODS set_log
      IMPORTING
        !io_log TYPE REF TO zcl_omp_log .
    METHODS get_num_attr
      IMPORTING
        !iv_atnam      TYPE atnam
      RETURNING
        VALUE(rs_attr) TYPE bapi1003_alloc_values_num .
    METHODS get_char_value_neutral
      IMPORTING
        !iv_atnam       TYPE atnam
      RETURNING
        VALUE(rv_atwrt) TYPE atwrt .
    METHODS get_character_attr
      IMPORTING
        !iv_atnam      TYPE atnam
      RETURNING
        VALUE(rs_attr) TYPE bapi1003_alloc_values_char .
    METHODS reset_log .
    CLASS-METHODS matnr_to_zdb_class
      IMPORTING
        !iv_matnr           TYPE matnr
      RETURNING
        VALUE(ro_doc_class) TYPE REF TO zcl_doc_class_data .
    CLASS-METHODS matnr_to_zdr_class
      IMPORTING
        !iv_matnr           TYPE matnr
      RETURNING
        VALUE(ro_doc_class) TYPE REF TO zcl_doc_class_data .
    METHODS get_linked_matnr
      EXPORTING
        !et_matnr TYPE tyt_matnr .
    METHODS exist_attribute
      IMPORTING
        !iv_atnam       TYPE atnam
      RETURNING
        VALUE(rv_exist) TYPE abap_bool .
    METHODS explode_document_bom
      EXPORTING
        !et_doc_bom_stpo TYPE stpo_tab .
    METHODS get_originale
      EXPORTING
        VALUE(et_files) TYPE tyt_doc_files .
  PROTECTED SECTION.
    TYPES: BEGIN OF tys_dokar_class,
             dokar TYPE dokar,
             class TYPE klasse_d,
           END OF tys_dokar_class,
           tyt_dokar_class TYPE STANDARD TABLE OF tys_dokar_class.

    TYPES:
      BEGIN OF tys_doc_class_map,
        doc_key TYPE bapi_doc_keys,
        class   TYPE klasse_d,
        obj     TYPE REF TO zcl_doc_class_data,
      END OF tys_doc_class_map .
    TYPES:
      tyt_doc_class_map TYPE STANDARD TABLE OF tys_doc_class_map .

    TYPES: BEGIN OF tys_fauf_doc_map,
             fauf  TYPE aufnr,
             dokar TYPE dokar,
             obj   TYPE REF TO zcl_doc_class_data,
           END OF tys_fauf_doc_map,
           tyt_fauf_doc_map TYPE STANDARD TABLE OF tys_fauf_doc_map.

    TYPES:
      BEGIN OF tys_matnr_doc_class_map,
        matnr   TYPE matnr,
        doc_key TYPE bapi_doc_keys,
        obj     TYPE REF TO zcl_doc_class_data,
      END OF tys_matnr_doc_class_map .
    TYPES:
      tyt_matnr_doc_class_map TYPE STANDARD TABLE OF tys_matnr_doc_class_map .

    DATA mv_class TYPE klasse_d .
    DATA mt_char TYPE tt_bapi1003_alloc_values_char .
    DATA mt_curr TYPE tt_bapi1003_alloc_values_curr .
    DATA mt_num TYPE tt_bapi1003_alloc_values_num .
    DATA ms_bapi_doc_key TYPE bapi_doc_keys .
    DATA mo_log TYPE REF TO zcl_omp_log .
    DATA: mt_originale      TYPE tyt_doc_files,
          mv_originale_read TYPE abap_bool.
    CLASS-DATA st_class_map TYPE tyt_doc_class_map .
    CLASS-DATA st_matnr_class_map TYPE tyt_matnr_doc_class_map .
    CLASS-DATA: st_dokar_class  TYPE tyt_dokar_class,
                st_fauf_doc_map TYPE tyt_fauf_doc_map.

    CLASS-METHODS: find_doc_by_fauf IMPORTING iv_fauf         TYPE aufnr
                                              it_search_attr  TYPE zif_omp=>tyt_atnam
                                              iv_dokar        TYPE dokar
                                    RETURNING VALUE(ro_class) TYPE REF TO zcl_doc_class_data .

    CLASS-METHODS: search_attr_in_configuration IMPORTING iv_cuobj       TYPE cuobj
                                                          it_search_attr TYPE zif_omp=>tyt_atnam
                                                RETURNING VALUE(rs_hit)  TYPE conf_out.

    CLASS-METHODS: get_config_objects IMPORTING iv_fauf       TYPE aufnr
                                      EXPORTING ev_vbap_cuobj TYPE vbap-cuobj
                                                ev_marc_cuobj TYPE marc-cuobj.



    CLASS-METHODS: dokar2class IMPORTING iv_dokar        TYPE dokar
                               RETURNING VALUE(rv_class) TYPE klasse_d.

    METHODS get_docname
      RETURNING
        VALUE(rv_msg) TYPE symsgv .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_doc_class_data IMPLEMENTATION.


  METHOD constructor.
*----------------------------------------------------
    DATA: lv_objkey TYPE objnum.
    DATA: lt_ret    TYPE TABLE OF bapiret2.
*----------------------------------------------------
    ms_bapi_doc_key = is_doc_key.
    mv_class = iv_class.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ms_bapi_doc_key-documentnumber
      IMPORTING
        output = ms_bapi_doc_key-documentnumber.

    CONCATENATE ms_bapi_doc_key-documenttype
                ms_bapi_doc_key-documentnumber
                ms_bapi_doc_key-documentversion
                ms_bapi_doc_key-documentpart INTO lv_objkey.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objkey
        objecttable     = 'DRAW'
        classnum        = mv_class
        classtype       = '017'
*       KEYDATE         = SY-DATUM
*       UNVALUATED_CHARS       = ' '
*       LANGUAGE        = SY-LANGU
*     IMPORTING
*       STATUS          =
*       STANDARDCLASS   =
      TABLES
        allocvaluesnum  = mt_num
        allocvalueschar = mt_char
        allocvaluescurr = mt_curr
        return          = lt_ret.

    /lpp/cx_exp_withlog=>raise_exception_from_rettab( it_ret = lt_ret ).

  ENDMETHOD.


  METHOD dokar2class.
*--------------------------------------
*--------------------------------------
    READ TABLE st_dokar_class ASSIGNING FIELD-SYMBOL(<info>)
      WITH KEY dokar = iv_dokar.
    IF sy-subrc NE 0.

      APPEND INITIAL LINE TO st_dokar_class ASSIGNING <info>.
      <info>-dokar = iv_dokar.
      SELECT SINGLE  klasse  INTO <info>-class
       FROM tdwa WHERE dokar = iv_dokar.

    ENDIF.

    rv_class = <info>-class.
  ENDMETHOD.


  METHOD exist_attribute.
*----------------------------------------------------
*----------------------------------------------------
    rv_exist = abap_true.
    READ TABLE mt_char TRANSPORTING NO FIELDS WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.

    READ TABLE mt_num TRANSPORTING NO FIELDS WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.

    READ TABLE mt_curr TRANSPORTING NO FIELDS WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0.
      RETURN.
    ENDIF.

    rv_exist = abap_false.

  ENDMETHOD.


  METHOD explode_document_bom.
*----------------------------------------------------
    DATA: lt_bom_hdr TYPE TABLE OF stko_api02,
          ls_bom_hdr TYPE stko_api02.
    DATA: lv_datum TYPE datuv_bi.
*----------------------------------------------------
    REFRESH et_doc_bom_stpo.

    WRITE sy-datum TO lv_datum.

    CALL FUNCTION 'DMU_DOC_BOM_EXPLODE'
      EXPORTING
        documenttype    = ms_bapi_doc_key-documenttype
        documentnumber  = ms_bapi_doc_key-documentnumber
        documentpart    = ms_bapi_doc_key-documentpart
        documentversion = ms_bapi_doc_key-documentversion
*       CHANGE_NUMBER   = CHANGE_NUMBER
        valid_from      = lv_datum
*       REVISION_LEVEL  = REVISION_LEVEL
        multi_level     = 'X'
*       GET_MATRICES    = 'X'
* IMPORTING
*       RETURN          = RETURN
      TABLES
        bomheader       = lt_bom_hdr
*       BOMITEMS        = BOMITEMS
*       documentdata    = et_bom_docs
*       DOCUMENTFILES   = DOCUMENTFILES
*       DMUPOSITIONOBJECTS       = DMUPOSITIONOBJECTS
*       DMUMATRICES     = DMUMATRICES
*       APPLICATIONS    = APPLICATIONS
      .
    READ TABLE lt_bom_hdr INTO ls_bom_hdr INDEX 1.
    CHECK sy-subrc EQ 0.

*   Positions-Nr werden gebraucht (fehlen im FUBA) -> stpo
    SELECT * INTO TABLE et_doc_bom_stpo
      FROM stpo
        WHERE
          stlty = 'D' AND
          stlnr = ls_bom_hdr-bom_no.

  ENDMETHOD.


  METHOD find_doc_by_fauf.
*--------------------------------------
*--------------------------------------
    CHECK it_search_attr IS NOT INITIAL.

    get_config_objects(
      EXPORTING
        iv_fauf       = iv_fauf
      IMPORTING
        ev_vbap_cuobj = DATA(lv_vbap_cuobj)
        ev_marc_cuobj = DATA(lv_marc_cuobj) ).

*   Suche in Kundenauftrags-Config
    DATA(ls_doc_attr) = search_attr_in_configuration( iv_cuobj = lv_vbap_cuobj
                                                      it_search_attr = it_search_attr ).
    IF ls_doc_attr IS INITIAL.
*    kein Treffer ->  Suche in Material-Config
      ls_doc_attr = search_attr_in_configuration( iv_cuobj = lv_marc_cuobj
                                                  it_search_attr = it_search_attr ).
    ENDIF.

    IF ls_doc_attr-atwrt IS NOT INITIAL.

      DATA(ls_doc_key) = zcl_omp_util=>atwrt2doc_key(
                           iv_atwrt = ls_doc_attr-atwrt
                           iv_dokar = iv_dokar ).

      ro_class = get_class( ls_doc_key ).

    ENDIF.

    APPEND INITIAL LINE TO st_fauf_doc_map ASSIGNING FIELD-SYMBOL(<map>).
    <map>-fauf = iv_fauf.
    <map>-dokar = iv_dokar.
    <map>-obj = ro_class.

  ENDMETHOD.


  METHOD find_zaz_doc_by_fauf.
*--------------------------------------
*--------------------------------------
    READ TABLE st_fauf_doc_map ASSIGNING FIELD-SYMBOL(<map>)
          WITH KEY fauf = iv_fauf
                   dokar = 'ZAZ'.
    IF sy-subrc EQ 0.
      ro_class = <map>-obj.
    ELSE.
      DATA: lt_attr TYPE zif_omp=>tyt_atnam.
      lt_attr = VALUE #(
                      ( 'ZLP_WP_MUSTER_DOK' )
                      ( 'ZLP_MT_MUSTER_DOK' )
                      ( 'ZLP_OF_MUSTER_DOK' ) ).

      ro_class = find_doc_by_fauf( iv_fauf        = iv_fauf
                                   it_search_attr = lt_attr
                                   iv_dokar       = 'ZAZ' ).
    ENDIF.

  ENDMETHOD.


  METHOD find_zdb_doc_by_fauf.
*--------------------------------------
*--------------------------------------
    READ TABLE st_fauf_doc_map ASSIGNING FIELD-SYMBOL(<map>)
          WITH KEY fauf = iv_fauf
                   dokar = 'ZDB'.
    IF sy-subrc EQ 0.
      ro_class = <map>-obj.
    ELSE.
      DATA: lt_attr TYPE zif_omp=>tyt_atnam.
      lt_attr = VALUE #(
                      ( 'ZLP_WP_DRUCKBILD_DOK' )
                      ( 'ZLP_MT_DRUCKBILD_DOK' )
                      ( 'ZLP_OF_DRUCKBILD_DOK' ) ).

      ro_class = find_doc_by_fauf( iv_fauf        = iv_fauf
                                   it_search_attr = lt_attr
                                   iv_dokar       = 'ZDB' ).
    ENDIF.
  ENDMETHOD.


  METHOD find_zdr_doc_by_fauf.
*--------------------------------------
*--------------------------------------
    READ TABLE st_fauf_doc_map ASSIGNING FIELD-SYMBOL(<map>)
          WITH KEY fauf = iv_fauf
                   dokar = 'ZDR'.
    IF sy-subrc EQ 0.
      ro_class = <map>-obj.
    ELSE.
      DATA: lt_attr TYPE zif_omp=>tyt_atnam.
      lt_attr = VALUE #(
                      ( 'ZLP_WP_DRUCKBILD_DOK_IS' )
                      ( 'ZLP_MT_DRUCKBILD_DOK_IS' )
                      ( 'ZLP_OF_DRUCKBILD_DOK_IS' ) ).

      ro_class = find_doc_by_fauf( iv_fauf        = iv_fauf
                                   it_search_attr = lt_attr
                                   iv_dokar       = 'ZDR' ).
    ENDIF.
  ENDMETHOD.


  METHOD find_zpv_doc_by_fauf.
*--------------------------------------
*--------------------------------------
    READ TABLE st_fauf_doc_map ASSIGNING FIELD-SYMBOL(<map>)
      WITH KEY fauf = iv_fauf
               dokar = 'ZPV'.
    IF sy-subrc EQ 0.
      ro_class = <map>-obj.
    ELSE.
      DATA: lt_attr TYPE zif_omp=>tyt_atnam.
      lt_attr = VALUE #(
                      ( 'ZLP_WP_PACK_DOK' )
                      ( 'ZLP_MT_PACK_DOK' ) ).

      ro_class = find_doc_by_fauf( iv_fauf        = iv_fauf
                                   it_search_attr = lt_attr
                                   iv_dokar       = 'ZPV' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_character_attr.
*----------------------------------------------------
    FIELD-SYMBOLS: <char> LIKE LINE OF mt_char.
*----------------------------------------------------
    CLEAR rs_attr.

    READ TABLE mt_char ASSIGNING <char>
         WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0.
      rs_attr = <char>.
    ELSE.
*     050   Dokument '&1': Merkmal '&2' (Klasse '&3') nicht gepflegt
      IF mo_log IS BOUND.  "kein Pflichfeld -> Warnung ins Log
        mo_log->add_warning( i_msgnr = '050' i_v1 = get_docname( ) i_v2 = iv_atnam i_v3 = mv_class ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_char_atwrt.
*----------------------------------------------------
    FIELD-SYMBOLS: <char> LIKE LINE OF mt_char.
*----------------------------------------------------
    CLEAR rv_atwrt.

    READ TABLE mt_char ASSIGNING <char>
         WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0 AND <char>-value_char IS NOT INITIAL.
      rv_atwrt = <char>-value_char.
    ELSE.
*     050   Dokument '&1': Merkmal '&2' (Klasse '&3') nicht gepflegt
      IF mo_log IS BOUND.  "kein Pflichfeld -> Warnung ins Log
        mo_log->add_warning( i_msgnr = '050' i_v1 = get_docname( ) i_v2 = iv_atnam i_v3 = mv_class ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_char_value_neutral.
*----------------------------------------------------
    FIELD-SYMBOLS: <char> LIKE LINE OF mt_char.
*----------------------------------------------------
    CLEAR rv_atwrt.

    READ TABLE mt_char ASSIGNING <char>
         WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0.
      rv_atwrt = <char>-value_neutral.
    ELSE.
*     050   Dokument '&1': Merkmal '&2' (Klasse '&3') nicht gepflegt
      IF mo_log IS BOUND.  "kein Pflichfeld -> Warnung ins Log
        mo_log->add_warning( i_msgnr = '050' i_v1 = get_docname( ) i_v2 = iv_atnam i_v3 = mv_class ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_class.
*----------------------------------------------------
    FIELD-SYMBOLS: <map> TYPE tys_doc_class_map.
*----------------------------------------------------
    CHECK is_doc_key IS NOT INITIAL.
    DATA(lv_class) = dokar2class( iv_dokar = is_doc_key-documenttype ).

    READ TABLE st_class_map ASSIGNING <map>
      WITH KEY
         doc_key = is_doc_key
         class = lv_class.
    IF sy-subrc NE 0.

      APPEND INITIAL LINE TO st_class_map ASSIGNING <map>.
      <map>-doc_key = is_doc_key.
      <map>-class = lv_class.

      TRY.
          CREATE OBJECT <map>-obj
            EXPORTING
              is_doc_key = is_doc_key
              iv_class   = lv_class.

        CATCH /lpp/cx_exp_withlog  ##NO_HANDLER.
      ENDTRY.

    ENDIF.
    ro_class = <map>-obj.

  ENDMETHOD.


  METHOD get_config_objects.
*--------------------------------------
*--------------------------------------
    CLEAR: ev_marc_cuobj,ev_vbap_cuobj.

    SELECT SINGLE
           aufnr,
           kdauf,
           kdpos,
           werks,
           plnbez
     INTO @DATA(ls_caufv)
    FROM caufv
      WHERE aufnr = @iv_fauf.

    IF ls_caufv-kdauf IS NOT INITIAL.
      SELECT SINGLE cuobj INTO ev_vbap_cuobj
        FROM vbap WHERE vbeln = ls_caufv-kdauf AND
                        posnr = ls_caufv-kdpos.
    ENDIF.

    SELECT SINGLE cuobj INTO ev_marc_cuobj
     FROM marc
       WHERE
         matnr = ls_caufv-plnbez AND
         werks = ls_caufv-werks.

  ENDMETHOD.


  METHOD get_docname.
*----------------------------------------------------
    DATA: lv_nr TYPE string.
*----------------------------------------------------
    lv_nr = ms_bapi_doc_key-documentnumber.
    SHIFT lv_nr LEFT DELETING LEADING '0'.
    CONCATENATE ms_bapi_doc_key-documenttype
                lv_nr
                ms_bapi_doc_key-documentpart
                ms_bapi_doc_key-documentversion INTO rv_msg SEPARATED BY '-'.

  ENDMETHOD.


  METHOD get_linked_matnr.
*----------------------------------------------------
    DATA: lt_drad TYPE TABLE OF drad.
    FIELD-SYMBOLS: <drad> TYPE drad.
*----------------------------------------------------
    REFRESH et_matnr.

    SELECT
      dokar
      doknr
      dokvr
      doktl
      dokob
      objky  INTO CORRESPONDING FIELDS OF TABLE lt_drad
    FROM drad
      WHERE
         dokar = ms_bapi_doc_key-documenttype AND
         doknr = ms_bapi_doc_key-documentnumber AND
         dokvr = ms_bapi_doc_key-documentversion AND
         doktl = ms_bapi_doc_key-documentpart AND
         dokob = 'MARA' AND
         delflag = space ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_drad ASSIGNING <drad>.
      APPEND <drad>-objky TO et_matnr.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_mandatory_char_attr.
*----------------------------------------------------
*----------------------------------------------------
    CLEAR rs_attr.

    READ TABLE mt_char INTO rs_attr
         WITH KEY charact = iv_atnam.
    IF sy-subrc NE 0  OR rs_attr-value_char IS INITIAL.
*     050   Dokument '&1': Merkmal '&2' (Klasse '&3') nicht gepflegt
      /lpp/cx_exp_withlog=>raise_exception(
            i_msg_class = zif_omp=>c_omp_msg_class
            i_msgnr     = '050'
            i_v1        = get_docname( )
            i_v2        = iv_atnam
            i_v3        = mv_class ).

    ENDIF.

  ENDMETHOD.


  METHOD get_mandatory_char_atwrt.
*----------------------------------------------------
    FIELD-SYMBOLS: <char> LIKE LINE OF mt_char.
*----------------------------------------------------
    CLEAR rv_atwrt.

    READ TABLE mt_char ASSIGNING <char>
         WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0 AND <char>-value_char IS NOT INITIAL.
      rv_atwrt = <char>-value_char.
    ELSE.
*    050  Dokument '&1': Merkmal '&2' (Klasse '&3') nicht gepflegt
      /lpp/cx_exp_withlog=>raise_exception(
            i_msg_class =  zif_omp=>c_omp_msg_class
            i_msgnr     = '050'
            i_v1        = get_docname( )
            i_v2        = iv_atnam
            i_v3        = mv_class ).

    ENDIF.

  ENDMETHOD.


  METHOD get_mandatory_num_attr.
*----------------------------------------------------
*----------------------------------------------------
    CLEAR rs_val.

    READ TABLE mt_num INTO rs_val
         WITH KEY charact = iv_atnam.
    IF sy-subrc NE 0.

*     050   Dokument '&1': Merkmal '&2' (Klasse '&3') nicht gepflegt
      /lpp/cx_exp_withlog=>raise_exception(
            i_msg_class = zif_omp=>c_omp_msg_class
            i_msgnr     = '050'
            i_v1        = get_docname( )
            i_v2        = iv_atnam
            i_v3        = mv_class ).

    ENDIF.

  ENDMETHOD.


  METHOD get_mandatory_num_atwrt.
*----------------------------------------------------
    FIELD-SYMBOLS: <num> LIKE LINE OF mt_num.
*----------------------------------------------------
    CLEAR rv_val.

    READ TABLE mt_num ASSIGNING <num>
         WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0.
      rv_val = <num>-value_from.
    ELSE.
*     050   Dokument '&1': Merkmal '&2' (Klasse '&3') nicht gepflegt
      /lpp/cx_exp_withlog=>raise_exception(
            i_msg_class = zif_omp=>c_omp_msg_class
            i_msgnr     = '050'
            i_v1        = get_docname( )
            i_v2        = iv_atnam
            i_v3        = mv_class ).

    ENDIF.

  ENDMETHOD.


  METHOD get_num_attr.
*----------------------------------------------------
    FIELD-SYMBOLS: <num> LIKE LINE OF mt_num.
*----------------------------------------------------
    CLEAR rs_attr.

    READ TABLE mt_num ASSIGNING <num>
         WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0.
      rs_attr = <num>.
    ELSE.
*     050  Dokument '&1': Merkmal '&2' (Klasse '&3') nicht gepflegt
      IF mo_log IS BOUND.  "kein Pflichtfeld -> Warnung ins Log
        mo_log->add_warning( i_msgnr = '050' i_v1 = get_docname( ) i_v2 = iv_atnam i_v3 = mv_class ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_num_attr_integer.
*----------------------------------------------------
    FIELD-SYMBOLS: <num> LIKE LINE OF mt_num.
*----------------------------------------------------
    CLEAR rv_val.

    READ TABLE mt_num ASSIGNING <num>
         WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0.
      rv_val = <num>-value_from.
    ELSE.
*     050   Dokument '&1': Merkmal '&2' (Klasse '&3') nicht gepflegt
      IF mo_log IS BOUND.  "kein Pflichtfeld -> Warnung ins Log
        mo_log->add_warning( i_msgnr = '050' i_v1 = get_docname( ) i_v2 = iv_atnam i_v3 = mv_class ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_originale.
*--------------------------------------
*--------------------------------------
    IF mv_originale_read EQ abap_true.
      et_files = mt_originale.
      RETURN.
    ENDIF.

    REFRESH et_files.

    CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
      EXPORTING
        documenttype       = ms_bapi_doc_key-documenttype
        documentnumber     = ms_bapi_doc_key-documentnumber
        documentpart       = ms_bapi_doc_key-documentpart
        documentversion    = ms_bapi_doc_key-documentversion
*       GETOBJECTLINKS     = ' '
*       GETCOMPONENTS      = ' '
*       GETSTATUSLOG       = ' '
*       GETLONGTEXTS       = ' '
        getactivefiles     = space
        getdocdescriptions = space
*       GETDOCFILES        = 'X'
*       GETCLASSIFICATION  = ' '
*       GETSTRUCTURE       = ' '
*       GETWHEREUSED       = ' '
*       HOSTNAME           = ' '
*       INHERITED          = 'X'
*       PF_BAPI_CALL       =
*      IMPORTING
*       documentdata       = data(ls_data)
*       return             = data(ls_return)
      TABLES
*       objectlinks        =
*       DOCUMENTDESCRIPTIONS =
*       LONGTEXTS          =
*       STATUSLOG          =
        documentfiles      = mt_originale
*       COMPONENTS         =
*       CHARACTERISTICVALUES       =
*       CLASSALLOCATIONS   =
*       DOCUMENTSTRUCTURE  =
*       WHEREUSEDLIST      =
      .
    mv_originale_read = abap_true.
    et_files = mt_originale.

  ENDMETHOD.


  METHOD matnr_to_zdb_class.
*----------------------------------------------------
    DATA: lt_drad TYPE TABLE OF drad.
    DATA: ls_doc_key TYPE bapi_doc_keys.
    FIELD-SYMBOLS: <drad> TYPE drad.
    FIELD-SYMBOLS: <map> TYPE tys_matnr_doc_class_map.
*----------------------------------------------------
    READ TABLE st_matnr_class_map ASSIGNING <map>
      WITH KEY matnr = iv_matnr
               doc_key-documenttype = 'ZDB'.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO st_matnr_class_map ASSIGNING <map>.
      <map>-matnr =  iv_matnr.

*     ZDB-Dokument suchen
      SELECT * INTO TABLE lt_drad
        FROM drad
          WHERE
            dokar = 'ZDB' AND
            dokob = 'MARA' AND
            objky = iv_matnr.

      READ TABLE lt_drad ASSIGNING <drad> INDEX 1.
      IF sy-subrc EQ 0.
        <map>-doc_key-documenttype = <drad>-dokar.
        <map>-doc_key-documentnumber = <drad>-doknr.
        <map>-doc_key-documentversion = <drad>-dokvr.
        <map>-doc_key-documentpart = <drad>-doktl.
        ls_doc_key = <map>-doc_key.
      ENDIF.

      IF ls_doc_key IS NOT INITIAL.
        <map>-obj = zcl_doc_class_data=>get_class( is_doc_key = ls_doc_key ).
      ENDIF.

    ENDIF.

    ro_doc_class = <map>-obj.

  ENDMETHOD.


  METHOD matnr_to_zdr_class.
*----------------------------------------------------
    DATA: lt_drad TYPE TABLE OF drad.
    DATA: ls_doc_key TYPE bapi_doc_keys.
    FIELD-SYMBOLS: <drad> TYPE drad.
    FIELD-SYMBOLS: <map> TYPE tys_matnr_doc_class_map.
*----------------------------------------------------
    READ TABLE st_matnr_class_map ASSIGNING <map>
      WITH KEY matnr = iv_matnr
               doc_key-documenttype = 'ZDR'.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO st_matnr_class_map ASSIGNING <map>.
      <map>-matnr =  iv_matnr.

*     ZDB-Dokument suchen
      SELECT * INTO TABLE lt_drad
        FROM drad
          WHERE
            dokar = 'ZDR' AND
            dokob = 'MARA' AND
            objky = iv_matnr.

      READ TABLE lt_drad ASSIGNING <drad> INDEX 1.
      IF sy-subrc EQ 0.
        <map>-doc_key-documenttype = <drad>-dokar.
        <map>-doc_key-documentnumber = <drad>-doknr.
        <map>-doc_key-documentversion = <drad>-dokvr.
        <map>-doc_key-documentpart = <drad>-doktl.
        ls_doc_key = <map>-doc_key.
      ENDIF.

      IF ls_doc_key IS NOT INITIAL.
        <map>-obj = zcl_doc_class_data=>get_class( is_doc_key = ls_doc_key ).
      ENDIF.

    ENDIF.

    ro_doc_class = <map>-obj.

  ENDMETHOD.


  METHOD reset_log.
*----------------------------------------------------
*----------------------------------------------------
    FREE mo_log.

  ENDMETHOD.


  METHOD search_attr_in_configuration.
*--------------------------------------
    DATA: lv_obj  TYPE cuobj.
    DATA: lt_attr TYPE    vlcconfig_t.
*--------------------------------------
    lv_obj = iv_cuobj.
    CALL FUNCTION 'VC_I_GET_CONFIGURATION_IBASE'
      EXPORTING
        instance           = lv_obj
*       business_object    =
*       language           = SY-LANGU
*       iv_invalid_possible =
*       iv_neutral         = space
      TABLES
        configuration      = lt_attr
*       et_conf_with_author =
      EXCEPTIONS
        instance_not_found = 1
        OTHERS             = 2.
    CHECK sy-subrc EQ 0.

    LOOP AT it_search_attr ASSIGNING FIELD-SYMBOL(<atnam>).

      LOOP AT lt_attr ASSIGNING FIELD-SYMBOL(<attr>)
        WHERE atnam = <atnam> AND atwrt IS NOT INITIAL.
        rs_hit = <attr>.
        RETURN.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_log.
*----------------------------------------------------
*----------------------------------------------------
    mo_log = io_log.

  ENDMETHOD.

  METHOD find_znp_doc_by_fauf.
*--------------------------------------
*--------------------------------------
    READ TABLE st_fauf_doc_map ASSIGNING FIELD-SYMBOL(<map>)
          WITH KEY fauf = iv_fauf
                   dokar = 'ZNP'.
    IF sy-subrc EQ 0.
      ro_class = <map>-obj.
    ELSE.
      DATA: lt_attr TYPE zif_omp=>tyt_atnam.
      lt_attr = VALUE #(
                      ( 'ZLP_WP_NUTZENPLAN_DOK' ) ).

      ro_class = find_doc_by_fauf( iv_fauf        = iv_fauf
                                   it_search_attr = lt_attr
                                   iv_dokar       = 'ZNP' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.