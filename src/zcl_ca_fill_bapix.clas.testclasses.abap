*"* use this source file for your ABAP unit test classes
CLASS ltcl_ca_fill_bapix DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS fill_structure_successful FOR TESTING RAISING cx_static_check.
    METHODS wrong_data_type FOR TESTING RAISING cx_static_check.
    METHODS wrong_datax_type FOR TESTING RAISING cx_static_check.
    METHODS data_deep_structure FOR TESTING RAISING cx_static_check.
    METHODS datax_deep_structure FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_ca_fill_bapix IMPLEMENTATION.
  METHOD fill_structure_successful.
    DATA bapimereqheader TYPE bapimereqheader.
    DATA bapimereqheaderx TYPE bapimereqheaderx.
    bapimereqheader = VALUE #( preq_no  = '0000000001'
                               pr_type  = 'NB'
                               ctrl_ind = 'T' ).
    DATA(expected_result) = VALUE bapimereqheaderx( preq_no  = abap_true
                                                    pr_type  = abap_true
                                                    ctrl_ind = abap_true ).
    TRY.
        zcl_ca_fill_bapix=>fill_bapix(
          EXPORTING
            bapi_data  = bapimereqheader
          CHANGING
            bapi_datax = bapimereqheaderx ).
        cl_abap_unit_assert=>assert_equals( act = bapimereqheaderx
                                            exp = expected_result
                                            msg = |Expected result not received| ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_bound( act = lx_ca_fill_bapix
                                           msg = |Exception instance shouldn't be bound| ).
  ENDMETHOD.

  METHOD wrong_data_type.
    DATA mandt TYPE mandt.
    DATA mandtx TYPE mandt.
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
            bapi_data  = mandt
          CHANGING
            bapi_datax = mandtx ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = lx_ca_fill_bapix->textid
            exp                  = lx_expected->textid ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act              = lx_ca_fill_bapix
        msg              = |Exception expected| ).
  ENDMETHOD.

  METHOD wrong_datax_type.
    DATA bapimereqheader TYPE bapimereqheader.
    DATA mandtx TYPE mandt.
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
            bapi_data  = bapimereqheader
          CHANGING
            bapi_datax = mandtx ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = lx_ca_fill_bapix->textid
            exp                  = lx_expected->textid ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act              = lx_ca_fill_bapix
        msg              = |Exception expected| ).
  ENDMETHOD.

  METHOD data_deep_structure.
    DATA:
      BEGIN OF data_deep_structure,
        BEGIN OF data_structure,
          mandt TYPE mandt,
        END OF data_structure,
      END OF data_deep_structure.
    DATA:
      BEGIN OF datax_structure,
        mandt TYPE mandt,
      END OF datax_structure.

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
            bapi_data  = data_deep_structure
          CHANGING
            bapi_datax = datax_structure ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = lx_ca_fill_bapix->textid
            exp                  = lx_expected->textid ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act              = lx_ca_fill_bapix
        msg              = |Exception expected| ).
  ENDMETHOD.

  METHOD datax_deep_structure.
    DATA:
      BEGIN OF data_structure,
        mandt TYPE mandt,
      END OF data_structure.
    DATA:
      BEGIN OF datax_deep_structure,
        BEGIN OF datax_structure,
          mandt TYPE mandt,
        END OF datax_structure,
      END OF datax_deep_structure.

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
            bapi_data  = data_structure
          CHANGING
            bapi_datax = datax_deep_structure ).
      CATCH zcx_ca_fill_bapix INTO DATA(lx_ca_fill_bapix).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  = lx_ca_fill_bapix->textid
            exp                  = lx_expected->textid ).
    ENDTRY.
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act              = lx_ca_fill_bapix
        msg              = |Exception expected| ).
  ENDMETHOD.
ENDCLASS.
