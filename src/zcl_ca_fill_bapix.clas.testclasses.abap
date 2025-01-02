CLASS ltcl_ca_fill_bapix DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS f01_data_ele_datax_ele_wr_type FOR TESTING RAISING cx_static_check.
    METHODS f02_data_tab_datax_tab_wr_type FOR TESTING RAISING cx_static_check.
    METHODS f03_data_tab_wr_type_datax_tab FOR TESTING RAISING cx_static_check.
    METHODS f04_data_str_datax_str_wr_type FOR TESTING RAISING cx_static_check.
    METHODS f05_data_tab_datax_ele_wr_type FOR TESTING RAISING cx_static_check.
    METHODS f06_data_dta_datax_tab_dee_str FOR TESTING RAISING cx_static_check.
    METHODS f07_data_dst_datax_str_dee_str FOR TESTING RAISING cx_static_check.
    METHODS f08_data_str_datax_dst_dee_str FOR TESTING RAISING cx_static_check.
    METHODS f09_data_tab_datax_dta_dee_str FOR TESTING RAISING cx_static_check.
    METHODS f10_data_tab_datax_tab_di_type FOR TESTING RAISING cx_static_check.

    METHODS s01_data_str_datax_str_fll_ddc FOR TESTING RAISING cx_static_check.
    METHODS s02_data_str_datax_str_fll_cmp FOR TESTING RAISING cx_static_check.
    METHODS s03_data_str_datax_str_fll_ddc FOR TESTING RAISING cx_static_check.
    METHODS s04_data_tab_datax_tab_fll_cmp FOR TESTING RAISING cx_static_check.
    METHODS s05_data_str_datax_str_no_data FOR TESTING RAISING cx_static_check.
    METHODS s06_data_tab_datax_tab_no_data FOR TESTING RAISING cx_static_check.
    METHODS s07_data_str_datax_str_as_incl FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_ca_fill_bapix IMPLEMENTATION.
  METHOD f01_data_ele_datax_ele_wr_type.
    DATA bapi_data TYPE mandt.
    DATA bapi_datax TYPE mandt.

    bapi_data = sy-mandt.
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
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_ca_fill_bapix->get_text( )
            exp = lx_expected->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_ca_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD f02_data_tab_datax_tab_wr_type.
    TYPES:
      BEGIN OF ty_bapi_data,
        client TYPE mandt,
      END OF ty_bapi_data.
    TYPES: ty_bapi_datax TYPE mandt.

    DATA bapi_data TYPE STANDARD TABLE OF ty_bapi_data.
    DATA bapi_datax TYPE STANDARD TABLE OF ty_bapi_datax.

    bapi_data = VALUE #( ( client =  sy-mandt ) ).
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
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_ca_fill_bapix->get_text( )
            exp = lx_expected->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_ca_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD f03_data_tab_wr_type_datax_tab.
    TYPES: ty_bapi_data TYPE mandt.
    TYPES:
      BEGIN OF ty_bapi_datax,
        client TYPE mandt,
      END OF ty_bapi_datax.

    DATA bapi_data TYPE STANDARD TABLE OF ty_bapi_data.
    DATA bapi_datax TYPE STANDARD TABLE OF ty_bapi_datax.

    bapi_data = VALUE #( ( sy-mandt ) ).
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
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_ca_fill_bapix->get_text( )
            exp = lx_expected->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_ca_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD f04_data_str_datax_str_wr_type.
    TYPES:
      BEGIN OF ty_bapi_data,
        client TYPE mandt,
      END OF ty_bapi_data.
    TYPES: ty_bapi_datax TYPE mandt.

    DATA bapi_data TYPE ty_bapi_data.
    DATA bapi_datax TYPE ty_bapi_datax.

    bapi_data = VALUE #( client =  sy-mandt ).
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
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_ca_fill_bapix->get_text( )
            exp = lx_expected->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_ca_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD f05_data_tab_datax_ele_wr_type.
    TYPES:
      BEGIN OF ty_data_structure,
        client TYPE mandt,
      END OF ty_data_structure.

    DATA bapi_data TYPE STANDARD TABLE OF ty_data_structure.
    DATA bapi_datax TYPE mandt.

    bapi_data = VALUE #( ( client =  sy-mandt ) ).
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
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_ca_fill_bapix->get_text( )
            exp = lx_expected->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_ca_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD f06_data_dta_datax_tab_dee_str.
    TYPES:
      BEGIN OF ty_bapi_data,
        BEGIN OF bapi_data,
          client TYPE mandt,
        END OF bapi_data,
      END OF ty_bapi_data.
    TYPES:
      BEGIN OF ty_bapi_datax,
        client TYPE mandt,
      END OF ty_bapi_datax.

    DATA bapi_data TYPE STANDARD TABLE OF ty_bapi_data.
    DATA bapi_datax TYPE STANDARD TABLE OF ty_bapi_datax.

    bapi_data = VALUE #( ( bapi_data-client = sy-mandt ) ).
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
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_ca_fill_bapix->get_text( )
            exp = lx_expected->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_ca_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD f07_data_dst_datax_str_dee_str.
    TYPES:
      BEGIN OF ty_bapi_data,
        BEGIN OF bapi_data,
          client TYPE mandt,
        END OF bapi_data,
      END OF ty_bapi_data.
    TYPES:
      BEGIN OF ty_bapi_datax,
        client TYPE mandt,
      END OF ty_bapi_datax.

    DATA bapi_data TYPE ty_bapi_data.
    DATA bapi_datax TYPE ty_bapi_datax.

    bapi_data = VALUE #( bapi_data-client = sy-mandt ).
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
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_ca_fill_bapix->get_text( )
            exp = lx_expected->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_ca_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD f08_data_str_datax_dst_dee_str.
    TYPES:
      BEGIN OF ty_bapi_data,
        client TYPE mandt,
      END OF ty_bapi_data.
    TYPES:
      BEGIN OF ty_bapi_datax,
        BEGIN OF datax_structure,
          client TYPE mandt,
        END OF datax_structure,
      END OF ty_bapi_datax.

    DATA bapi_data TYPE ty_bapi_data.
    DATA bapi_datax TYPE ty_bapi_datax.

    bapi_data = VALUE #( client = sy-mandt ).
    TRY.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
            parameter_name = 'BAPI_DATAX'.
      CATCH zcx_ca_fill_bapix INTO DATA(lx_expected).
    ENDTRY.
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_ca_fill_bapix->get_text( )
            exp = lx_expected->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_ca_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD f09_data_tab_datax_dta_dee_str.
    TYPES:
      BEGIN OF ty_bapi_data,
        client TYPE mandt,
      END OF ty_bapi_data.
    TYPES:
      BEGIN OF ty_bapi_datax,
        BEGIN OF datax_structure,
          client TYPE mandt,
        END OF datax_structure,
      END OF ty_bapi_datax.

    DATA bapi_data TYPE STANDARD TABLE OF ty_bapi_data.
    DATA bapi_datax TYPE STANDARD TABLE OF ty_bapi_datax.

    bapi_data = VALUE #( ( client =  sy-mandt ) ).
    TRY.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
            parameter_name = 'BAPI_DATAX'.
      CATCH zcx_ca_fill_bapix INTO DATA(lx_expected).
    ENDTRY.
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_ca_fill_bapix->get_text( )
            exp = lx_expected->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_ca_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD f10_data_tab_datax_tab_di_type.
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

    bapi_data = VALUE #( ( client =  sy-mandt ) ).
    TRY.
        RAISE EXCEPTION TYPE zcx_ca_fill_bapix
          EXPORTING
            textid            = zcx_ca_fill_bapix=>different_types
            parameter_name_01 = 'BAPI_DATA'
            parameter_name_02 = 'BAPI_DATAX'.
      CATCH zcx_ca_fill_bapix INTO DATA(lx_expected).
    ENDTRY.
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lx_ca_fill_bapix->get_text( )
            exp = lx_expected->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lx_ca_fill_bapix
        msg = |Exception expected| ).
  ENDMETHOD.

  METHOD s01_data_str_datax_str_fll_ddc.
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
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_ca_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.

  METHOD s02_data_str_datax_str_fll_cmp.
    TYPES:
      BEGIN OF ty_data,
        client        TYPE mandt,
        source_client TYPE mandt,
      END OF ty_data.

    TYPES:
      BEGIN OF ty_datax,
        client        TYPE abap_bool,
        source_client TYPE mandt,
        update        TYPE abap_bool,
      END OF ty_datax.

    DATA bapi_datax TYPE ty_datax.
    DATA(bapi_data) = VALUE ty_data( client = sy-mandt ).
    DATA(expected_result) = VALUE ty_datax( client = abap_true ).
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
        cl_abap_unit_assert=>assert_equals( act = bapi_datax
                                            exp = expected_result
                                            msg = |Expected result not received| ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_ca_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.

  METHOD s03_data_str_datax_str_fll_ddc.
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
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_ca_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.

  METHOD s04_data_tab_datax_tab_fll_cmp.
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
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_ca_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.

  METHOD s05_data_str_datax_str_no_data.
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

    DATA bapi_data TYPE tty_data.
    DATA bapi_datax TYPE tty_datax.
    DATA(expected_result) = VALUE tty_datax( ).
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
        cl_abap_unit_assert=>assert_equals( act = bapi_datax
                                            exp = expected_result
                                            msg = |Expected result not received| ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_ca_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.

  METHOD s06_data_tab_datax_tab_no_data.
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
    DATA(bapi_data) = VALUE tty_data( ( ) ).
    DATA(expected_result) = VALUE tty_datax( ( ) ).
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
        cl_abap_unit_assert=>assert_equals( act = bapi_datax
                                            exp = expected_result
                                            msg = |Expected result not received| ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_ca_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.

  METHOD s07_data_str_datax_str_as_incl.
    TYPES:
      BEGIN OF ty_data,
        client        TYPE mandt,
        source_client TYPE mandt,
      END OF ty_data.
    TYPES:
      BEGIN OF ty_data_as_incl.
        INCLUDE TYPE ty_data AS incl.
    TYPES:
      END OF ty_data_as_incl.
    TYPES tty_data_as_incl TYPE STANDARD TABLE OF ty_data_as_incl WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_datax,
        client        TYPE abap_bool,
        source_client TYPE mandt,
        update        TYPE abap_bool,
      END OF ty_datax.
    TYPES:
      BEGIN OF ty_datax_as_incl.
        INCLUDE TYPE ty_data AS incl.
    TYPES:
      END OF ty_datax_as_incl.
    TYPES tty_datax_as_incl TYPE STANDARD TABLE OF ty_datax_as_incl WITH DEFAULT KEY.

    DATA bapi_data TYPE tty_data_as_incl.
    DATA bapi_datax TYPE tty_datax_as_incl.
    bapi_data = VALUE tty_datax_as_incl( ( source_client = '100' ) ).
    DATA(expected_result) = VALUE tty_datax_as_incl( ( source_client = '100' ) ).
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
        cl_abap_unit_assert=>assert_equals( act = bapi_datax
                                            exp = expected_result
                                            msg = |Expected result not received| ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_ca_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.
ENDCLASS.
