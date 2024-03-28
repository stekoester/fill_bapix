*"* use this source file for your ABAP unit test classes
CLASS ltcl_ca_fill_bapix DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS successful_fill_structure FOR TESTING RAISING cx_static_check.
    METHODS successful_fill_table FOR TESTING RAISING cx_static_check.
    METHODS successful_fill_table_comp FOR TESTING RAISING cx_static_check.
    METHODS fail_wrong_data_type FOR TESTING RAISING cx_static_check.
    METHODS fail_wrong_datax_type FOR TESTING RAISING cx_static_check.
    METHODS fail_data_deep_structure FOR TESTING RAISING cx_static_check.
    METHODS fail_datax_deep_structure FOR TESTING RAISING cx_static_check.
    METHODS fail_different_types FOR TESTING RAISING cx_static_check.
    METHODS fail_table_wrong_datax_type FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_ca_fill_bapix IMPLEMENTATION.
  METHOD successful_fill_structure.
    DATA bapi_data TYPE bapimereqheader.
    DATA bapi_datax TYPE bapimereqheaderx.
    bapi_data = VALUE #( preq_no  = '0000000001'
                         pr_type  = 'NB'
                         ctrl_ind = 'T' ).
    DATA(expected_result) = VALUE bapimereqheaderx( preq_no  = abap_true
                                                    pr_type  = abap_true
                                                    ctrl_ind = abap_true ).
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
        cl_abap_unit_assert=>assert_equals( act                  = bapi_datax
                                            exp                  = expected_result
                                            msg                  = |Expected result not received|
                                            ignore_hash_sequence = abap_true ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_stkoes_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_stkoes_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.

  METHOD successful_fill_table.
    DATA bapi_data  TYPE ty_bapimereqitemimp.
    DATA bapi_datax TYPE ty_bapimereqitemx.
    bapi_data = VALUE #( ( preq_item = '00010'
                           pur_group = '001'
                           short_text = 'Material description'
                           material = 'Material' ) ).
    DATA(expected_result) = VALUE ty_bapimereqitemx( ( preq_item = '00010'
                                                       pur_group = abap_true
                                                       short_text = abap_true
                                                       material = abap_true ) ).
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
        cl_abap_unit_assert=>assert_equals( act = bapi_datax
                                            exp = expected_result
                                            msg = |Expected result not received| ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_stkoes_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_stkoes_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.

  METHOD successful_fill_table_comp.
    TYPES:
      BEGIN OF ty_data,
        client        TYPE mandt,
        source_client TYPE mandt,
      END OF ty_data.
    TYPES tty_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_datax,
        client        TYPE abap_bool,
        source_client TYPE mandt,
        update        TYPE abap_bool,
      END OF ty_datax.
    TYPES tty_datax TYPE STANDARD TABLE OF ty_datax WITH DEFAULT KEY.

    DATA bapi_datax TYPE tty_datax.
    DATA(bapi_data) = VALUE tty_data( ( client = sy-mandt ) ).
    DATA(expected_result) = VALUE tty_datax( ( client = abap_true ) ).
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
        cl_abap_unit_assert=>assert_equals( act = bapi_datax
                                            exp = expected_result
                                            msg = |Expected result not received| ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_stkoes_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_stkoes_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.

  METHOD fail_wrong_data_type.
    DATA bapi_data TYPE mandt.
    DATA bapi_datax TYPE mandt.
    TRY.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>parameter_wrong_type
            parameter_name = 'BAPI_DATA'.
      CATCH zcx_ca_fill_bapix INTO DATA(lx_expected).
    ENDTRY.
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_stkoes_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_stkoes_fill_bapix->textid
            exp = lx_expected->textid ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_stkoes_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD fail_wrong_datax_type.
    DATA bapi_data TYPE bapimereqheader.
    DATA bapi_datax TYPE mandt.
    TRY.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>parameter_wrong_type
            parameter_name = 'BAPI_DATAX'.
      CATCH zcx_ca_fill_bapix INTO DATA(lx_expected).
    ENDTRY.
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_stkoes_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_stkoes_fill_bapix->textid
            exp = lx_expected->textid ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_stkoes_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD fail_data_deep_structure.
    DATA:
      BEGIN OF bapi_data,
        BEGIN OF data_structure,
          client TYPE mandt,
        END OF data_structure,
      END OF bapi_data.
    DATA:
      BEGIN OF bapi_datax,
        client TYPE mandt,
      END OF bapi_datax.

    TRY.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
            parameter_name = 'BAPI_DATA'.
      CATCH zcx_ca_fill_bapix INTO DATA(lx_expected).
    ENDTRY.
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_stkoes_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_stkoes_fill_bapix->textid
            exp = lx_expected->textid ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_stkoes_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD fail_datax_deep_structure.
    DATA:
      BEGIN OF bapi_data,
        client TYPE mandt,
      END OF bapi_data.
    DATA:
      BEGIN OF bapi_datax,
        BEGIN OF datax_structure,
          client TYPE mandt,
        END OF datax_structure,
      END OF bapi_datax.

    TRY.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
            parameter_name = 'BAPI_DATA'.
      CATCH zcx_ca_fill_bapix INTO DATA(lx_expected).
    ENDTRY.
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_stkoes_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_stkoes_fill_bapix->textid
            exp = lx_expected->textid ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_stkoes_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD fail_different_types.
    TYPES:
      BEGIN OF ty_data_structure,
        client TYPE mandt,
      END OF ty_data_structure.
    TYPES:
      BEGIN OF ty_datax_structure,
        client TYPE mandt,
      END OF ty_datax_structure.

    DATA bapi_data TYPE STANDARD TABLE OF ty_data_structure.
    DATA bapi_datax TYPE ty_datax_structure.

    TRY.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
            parameter_name = 'BAPI_DATA'.
      CATCH zcx_ca_fill_bapix INTO DATA(lx_expected).
    ENDTRY.
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_stkoes_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_stkoes_fill_bapix->textid
            exp = lx_expected->textid ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_stkoes_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD fail_table_wrong_datax_type.
    TYPES:
      BEGIN OF ty_data_structure,
        client TYPE mandt,
      END OF ty_data_structure.

    DATA bapi_data TYPE STANDARD TABLE OF ty_data_structure.
    DATA bapi_datax TYPE mandt.

    TRY.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
            parameter_name = 'BAPI_DATA'.
      CATCH zcx_ca_fill_bapix INTO DATA(lx_expected).
    ENDTRY.
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_stkoes_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_stkoes_fill_bapix->textid
            exp = lx_expected->textid ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_stkoes_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.
ENDCLASS.
