CLASS zcl_arbpl_class_data DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_arbpl TYPE arbpl
        !iv_werk  TYPE werks_d
        !iv_class TYPE klasse_d
        !iv_klart TYPE klassenart DEFAULT '019' .

    METHODS get_char_attr
      IMPORTING
        !iv_atnam      TYPE atnam
      RETURNING
        VALUE(rs_attr) TYPE bapi1003_alloc_values_char.

    METHODS get_char_atwrt
      IMPORTING
        !iv_atnam       TYPE atnam
      RETURNING
        VALUE(rv_atwrt) TYPE atwrt .

    METHODS get_num_attr_mult
      IMPORTING
        !iv_atnam     TYPE atnam
      RETURNING
        VALUE(rt_val) TYPE tt_bapi1003_alloc_values_num.

    METHODS get_num_attr
      IMPORTING
        !iv_atnam     TYPE atnam
      RETURNING
        VALUE(rs_val) TYPE bapi1003_alloc_values_num .

    CLASS-METHODS: get_machine_nr IMPORTING iv_arbpl             TYPE arbpl
                                            iv_werk              TYPE werks_d
                                  RETURNING VALUE(rv_machine_nr) TYPE zif_omp=>ty_maschine_nr.
    CLASS-METHODS get_class
      IMPORTING
        !iv_arbpl       TYPE arbpl
        !iv_werk        TYPE werks_d
        !iv_class       TYPE klasse_d
        !iv_klart       TYPE klassenart DEFAULT '019'
      RETURNING
        VALUE(ro_class) TYPE REF TO zcl_arbpl_class_data .

    CLASS-METHODS maschinenr_to_arbpl
      IMPORTING !iv_maschine_nr TYPE zif_omp=>ty_maschine_nr
      RETURNING VALUE(rt_crhd)  TYPE tt_crhd .

  PROTECTED SECTION.

    TYPES: BEGIN OF tys_maschine,
             maschine_nr TYPE zif_omp=>ty_maschine_nr,
             crhd_tab    TYPE tt_crhd,
           END OF tys_maschine,
           tyt_maschine TYPE STANDARD TABLE OF tys_maschine WITH EMPTY KEY.

    TYPES:
      BEGIN OF tys_class_map,
        arbpl TYPE  arbpl,
        werk  TYPE  werks_d,
        class TYPE  klasse_d,
        klart TYPE  klassenart,
        obj   TYPE REF TO zcl_arbpl_class_data,
      END OF tys_class_map .
    TYPES:
      tyt_class_map TYPE STANDARD TABLE OF tys_class_map .

    DATA mv_class TYPE klasse_d .
    DATA mt_char TYPE tt_bapi1003_alloc_values_char .
    DATA mt_curr TYPE tt_bapi1003_alloc_values_curr .
    DATA mt_num TYPE tt_bapi1003_alloc_values_num .
    DATA mv_arbpl TYPE arbpl .
    CLASS-DATA: st_class_map   TYPE tyt_class_map,
                st_machine_map TYPE tyt_maschine.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_arbpl_class_data IMPLEMENTATION.


  METHOD constructor.
*----------------------------------------------------
    DATA: lv_objkey TYPE objnum.
    DATA: lt_ret    TYPE TABLE OF bapiret2.
    DATA: BEGIN OF ls_clas_arbpl,
            werks TYPE crhd-werks,
            arbpl TYPE crhd-arbpl,
          END OF ls_clas_arbpl.
*----------------------------------------------------
    mv_arbpl = iv_arbpl.

    mv_class = iv_class.
    ls_clas_arbpl-werks = iv_werk.
    ls_clas_arbpl-arbpl = mv_arbpl.
    lv_objkey = ls_clas_arbpl.

    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objkey
        objecttable     = 'CRHD'
        classnum        = mv_class
        classtype       = iv_klart
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

*    /lpp/cx_exp_withlog=>raise_exception_from_rettab( it_ret = lt_ret ).

  ENDMETHOD.


  METHOD maschinenr_to_arbpl.
*----------------------------------------------------
    DATA: lv_atinn TYPE atinn.
    DATA: lt_ausp TYPE TABLE OF ausp.
    DATA: lv_maschine_nr TYPE zif_omp=>ty_maschine_nr.
    FIELD-SYMBOLS: <ausp> TYPE ausp.
    DATA: ls_crhd   TYPE crhd,
          lv_objnum TYPE objnum.
    DATA: lt_hits TYPE tt_crhd.
*----------------------------------------------------
    CHECK iv_maschine_nr IS NOT INITIAL.

    READ TABLE st_machine_map ASSIGNING FIELD-SYMBOL(<map>)
     WITH KEY maschine_nr = iv_maschine_nr.
    IF sy-subrc EQ 0.
      rt_crhd = <map>-crhd_tab.
      RETURN.
    ENDIF.

    SELECT SINGLE atinn INTO lv_atinn
      FROM cabn WHERE atnam = zif_omp=>cs_arbpl-maschinennr.
    CHECK sy-subrc EQ 0.

    SELECT * INTO TABLE lt_ausp
      FROM ausp
         WHERE klart = '019' AND
               atinn = lv_atinn.

    LOOP AT lt_ausp ASSIGNING <ausp>.
      lv_maschine_nr = <ausp>-atwrt.
      IF lv_maschine_nr EQ iv_maschine_nr.
        SELECT SINGLE cuobj,obtab, objek INTO @DATA(ls_inob)
         FROM inob
           WHERE cuobj = @<ausp>-objek AND obtab = 'CRHD'.

        CHECK sy-subrc EQ 0.
        ls_crhd-werks = ls_inob-objek+0(4).
        SHIFT ls_inob-objek BY 4 PLACES.
        ls_crhd-arbpl = ls_inob-objek.
        APPEND ls_crhd TO lt_hits.
      ENDIF.

    ENDLOOP.

    CHECK lt_hits IS NOT INITIAL.

    SELECT * INTO TABLE rt_crhd
      FROM crhd FOR ALL ENTRIES IN lt_hits
        WHERE
          arbpl = lt_hits-arbpl AND
          werks = lt_hits-werks.

    APPEND INITIAL LINE TO st_machine_map ASSIGNING <map>.
    <map>-maschine_nr = iv_maschine_nr.
    <map>-crhd_tab = rt_crhd.

  ENDMETHOD.


  METHOD get_char_atwrt.
*----------------------------------------------------
    FIELD-SYMBOLS: <char> LIKE LINE OF mt_char.
*----------------------------------------------------
    READ TABLE mt_char ASSIGNING <char>
         WITH KEY charact = iv_atnam.
    IF sy-subrc EQ 0 AND <char>-value_char IS NOT INITIAL.
      rv_atwrt = <char>-value_char.
    ENDIF.

  ENDMETHOD.


  METHOD get_class.
*----------------------------------------------------
    FIELD-SYMBOLS: <map> LIKE LINE OF st_class_map.
*----------------------------------------------------
    READ TABLE st_class_map ASSIGNING <map>
      WITH KEY
          arbpl = iv_arbpl
          werk  = iv_werk
          class = iv_class
          klart = iv_klart.
    IF sy-subrc NE 0.
      APPEND INITIAL LINE TO st_class_map ASSIGNING <map>.
      <map>-arbpl = iv_arbpl.
      <map>-werk = iv_werk.
      <map>-class = iv_class.
      <map>-klart = iv_klart.

      CREATE OBJECT <map>-obj
        EXPORTING
          iv_arbpl = iv_arbpl
          iv_werk  = iv_werk
          iv_class = iv_class
          iv_klart = iv_klart.

    ENDIF.

    ro_class = <map>-obj.

  ENDMETHOD.


  METHOD get_num_attr.
*----------------------------------------------------
*----------------------------------------------------
    READ TABLE mt_num INTO rs_val
         WITH KEY charact = iv_atnam.

  ENDMETHOD.




  METHOD get_machine_nr.
*--------------------------------------
*--------------------------------------
    DATA(lo_class) =   get_class(  iv_arbpl = iv_arbpl
                                   iv_werk  = iv_werk
                                   iv_class = zif_omp=>cs_arbpl-class ).
    CHECK lo_class IS BOUND.
    DATA(lv_atwrt) = lo_class->get_char_atwrt( zif_omp=>cs_arbpl-maschinennr ).
    SHIFT lv_atwrt LEFT DELETING LEADING space.
    rv_machine_nr = lv_atwrt.

  ENDMETHOD.

  METHOD get_num_attr_mult.
*--------------------------------------
*--------------------------------------
    LOOP AT mt_num ASSIGNING FIELD-SYMBOL(<num>)
     WHERE charact = iv_atnam.
      APPEND <num> TO rt_val.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_char_attr.
*----------------------------------------------------
    FIELD-SYMBOLS: <char> LIKE LINE OF mt_char.
*----------------------------------------------------
    READ TABLE mt_char INTO rs_attr WITH KEY charact = iv_atnam.

  ENDMETHOD.

ENDCLASS.