CLASS zcl_ca_fill_bapix DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Fill BAPIX data dependent on BAPI data</p>
    "!
    "! @parameter bapi_data         | <p class="shorttext synchronized">BAPI data</p>
    "! @parameter bapi_datax        | <p class="shorttext synchronized">BAPIX data</p>
    "! @raising   zcx_ca_fill_bapix | <p class="shorttext synchronized">Exceptions for zcx_ca_fill_bapix</p>
    CLASS-METHODS fill_bapix
      IMPORTING
        bapi_data         TYPE data
      CHANGING
        VALUE(bapi_datax) TYPE data
      RAISING
        zcx_ca_fill_bapix.

protected section.
*"* protected components of class ZCL_CA_FILL_BAPIX
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_CA_FILL_BAPIX
*"* do not include other source files here!!!

  class-methods _FILL_BAPIX_STRUCTURE
    importing
      !BAPI_DATA type DATA
    changing
      !BAPI_DATAX type DATA
    raising
      ZCX_CA_FILL_BAPIX .
  class-methods _FILL_BAPIX_TABLE
    importing
      !BAPI_DATA type DATA
    changing
      !BAPI_DATAX type DATA
    raising
      ZCX_CA_FILL_BAPIX .
  class-methods _FILL_BAPIX_STRUCTURE_BY_DDIC
    importing
      !BAPI_DATA type DATA
    changing
      !BAPI_DATAX type DATA .
  class-methods _FILL_BAPIX_STRUCTURE_BY_COMP
    importing
      !BAPI_DATA type DATA
    changing
      !BAPI_DATAX type DATA
    raising
      ZCX_CA_FILL_BAPIX .
  type-pools ABAP .
  class-methods _GET_PARAMETER_KIND
    importing
      !BAPI_DATA type DATA
      !BAPI_DATAX type DATA
    returning
      value(PARAMETER_KIND) type ABAP_TYPECATEGORY
    raising
      ZCX_CA_FILL_BAPIX .
  class-methods _IS_FLAT
    importing
      !ABAP_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR
    returning
      value(IS_FLAT) type ABAP_BOOL .
  class-methods _GET_COMPONENTS
    importing
      !ABAP_TYPEDESCR type ref to CL_ABAP_TYPEDESCR
    returning
      value(COMPONENTS) type ABAP_COMPONENT_TAB .
ENDCLASS.



CLASS ZCL_CA_FILL_BAPIX IMPLEMENTATION.


  METHOD fill_bapix.
    IF bapi_data IS INITIAL.
      RETURN.
    ENDIF.

    CASE _get_parameter_kind( bapi_data  = bapi_data
                              bapi_datax = bapi_datax ).
      WHEN cl_abap_typedescr=>kind_struct.
        _fill_bapix_structure(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
      WHEN cl_abap_typedescr=>kind_table.
        _fill_bapix_table(
          EXPORTING
            bapi_data  = bapi_data
          CHANGING
            bapi_datax = bapi_datax ).
    ENDCASE.
  ENDMETHOD.


  METHOD _fill_bapix_structure.
    IF bapi_data IS INITIAL.
      RETURN.
    ENDIF.
    IF cl_abap_structdescr=>describe_by_data( bapi_data )->is_ddic_type( ) EQ abap_true AND
       cl_abap_structdescr=>describe_by_data( bapi_datax )->is_ddic_type( ) EQ abap_true.
      _fill_bapix_structure_by_ddic(
        EXPORTING
          bapi_data  = bapi_data
        CHANGING
          bapi_datax = bapi_datax ).
    ELSE.
      _fill_bapix_structure_by_comp(
        EXPORTING
          bapi_data  = bapi_data
        CHANGING
          bapi_datax = bapi_datax ).
    ENDIF.
  ENDMETHOD.


  METHOD _fill_bapix_structure_by_comp.
    DATA bapi_data_component TYPE REF TO abap_componentdescr.
    DATA bapi_datax_component TYPE REF TO abap_componentdescr.

    DATA bapi_data_components TYPE abap_component_tab.
    DATA bapi_datax_components TYPE abap_component_tab.

    DATA bapi_data_structdescr TYPE REF TO cl_abap_structdescr.
    DATA bapi_datax_structdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS <bapi_data_value> TYPE data.
    FIELD-SYMBOLS <bapi_datax_value> TYPE data.

    bapi_data_structdescr ?= cl_abap_structdescr=>describe_by_data( bapi_data ).
    bapi_datax_structdescr ?= cl_abap_structdescr=>describe_by_data( bapi_datax ).

    bapi_data_components = _get_components( bapi_data_structdescr ).
    bapi_datax_components = _get_components( bapi_datax_structdescr ).

    LOOP AT bapi_datax_components REFERENCE INTO bapi_datax_component.
      READ TABLE bapi_data_components WITH KEY name = bapi_datax_component->name
           REFERENCE INTO bapi_data_component.
      IF sy-subrc EQ 4.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT bapi_data_component->name OF STRUCTURE bapi_data
             TO <bapi_data_value>.
      ASSIGN COMPONENT bapi_datax_component->name OF STRUCTURE bapi_datax
             TO <bapi_datax_value>.
      IF bapi_data_component->type->absolute_name EQ bapi_datax_component->type->absolute_name.
        <bapi_datax_value> = <bapi_data_value>.
      ELSE.
        IF <bapi_data_value> IS NOT INITIAL.
          <bapi_datax_value> = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD _fill_bapix_structure_by_ddic.
    DATA bapi_data_ddic_field TYPE REF TO dfies.
    DATA bapi_datax_ddic_field TYPE REF TO dfies.

    DATA bapi_data_ddic_fields TYPE ddfields.
    DATA bapi_datax_ddic_fields TYPE ddfields.

    DATA bapi_data_structdescr TYPE REF TO cl_abap_structdescr.
    DATA bapi_datax_structdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS <bapi_data_value> TYPE data.
    FIELD-SYMBOLS <bapi_datax_value> TYPE data.

    bapi_data_structdescr ?= cl_abap_structdescr=>describe_by_data( bapi_data ).
    bapi_datax_structdescr ?= cl_abap_structdescr=>describe_by_data( bapi_datax ).

    bapi_data_ddic_fields = bapi_data_structdescr->get_ddic_field_list( p_including_substructres = abap_true ).
    bapi_datax_ddic_fields = bapi_datax_structdescr->get_ddic_field_list( p_including_substructres = abap_true ).

    LOOP AT bapi_datax_ddic_fields REFERENCE INTO bapi_datax_ddic_field.
      READ TABLE bapi_data_ddic_fields WITH KEY fieldname = bapi_datax_ddic_field->fieldname
           REFERENCE INTO bapi_data_ddic_field.
      IF sy-subrc EQ 4.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT bapi_data_ddic_field->fieldname OF STRUCTURE bapi_data
             TO <bapi_data_value>.
      ASSIGN COMPONENT bapi_datax_ddic_field->fieldname OF STRUCTURE bapi_datax
             TO <bapi_datax_value>.
      IF bapi_data_ddic_field->rollname EQ bapi_datax_ddic_field->rollname.
        <bapi_datax_value> = <bapi_data_value>.
      ELSE.
        IF <bapi_data_value> IS NOT INITIAL.
          <bapi_datax_value> = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD _fill_bapix_table.
    FIELD-SYMBOLS <bapi_data_line> TYPE any.
    FIELD-SYMBOLS <bapi_datax_line> TYPE any.

    FIELD-SYMBOLS <bapi_data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <bapi_datax> TYPE STANDARD TABLE.

    ASSIGN bapi_data TO <bapi_data>.
    ASSIGN bapi_datax TO <bapi_datax>.

    LOOP AT <bapi_data> ASSIGNING <bapi_data_line>.
      APPEND INITIAL LINE TO <bapi_datax> ASSIGNING <bapi_datax_line>.
      _fill_bapix_structure(
        EXPORTING
          bapi_data  = <bapi_data_line>
        CHANGING
          bapi_datax = <bapi_datax_line> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_components.
    DATA insert_position TYPE i.

    DATA component TYPE REF TO abap_componentdescr.

    DATA abap_structdescr TYPE REF TO cl_abap_structdescr.

    IF abap_typedescr->kind NE cl_abap_typedescr=>kind_struct.
      RETURN.
    ENDIF.
    abap_structdescr ?= abap_typedescr.

    components = abap_structdescr->get_components( ).

    LOOP AT components REFERENCE INTO component
         WHERE as_include EQ abap_true.
      insert_position = sy-tabix.
      INSERT LINES OF _get_components( component->type )
             INTO components
             INDEX insert_position.
    ENDLOOP.
    DELETE components WHERE as_include EQ abap_true.
  ENDMETHOD.


  METHOD _get_parameter_kind.
    DATA valid_kinds TYPE char2.

    DATA bapi_data_typedescr TYPE REF TO cl_abap_typedescr.
    DATA bapi_datax_typedescr TYPE REF TO cl_abap_typedescr.

    DATA bapi_data_structdescr TYPE REF TO cl_abap_structdescr.
    DATA bapi_datax_structdescr TYPE REF TO cl_abap_structdescr.

    DATA bapi_data_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA bapi_datax_tabledescr TYPE REF TO cl_abap_tabledescr.

    bapi_data_typedescr = cl_abap_typedescr=>describe_by_data( bapi_data ).
    bapi_datax_typedescr = cl_abap_typedescr=>describe_by_data( bapi_datax ).
    CONCATENATE cl_abap_typedescr=>kind_struct cl_abap_typedescr=>kind_table INTO valid_kinds.
    IF bapi_data_typedescr->kind NA valid_kinds.
      RAISE EXCEPTION TYPE zcx_ca_fill_bapix
        EXPORTING
          textid         = zcx_ca_fill_bapix=>parameter_wrong_type
          parameter_name = 'BAPI_DATA'.
    ELSEIF bapi_datax_typedescr->kind NA |{ cl_abap_typedescr=>kind_struct }{ cl_abap_typedescr=>kind_table }|.
      RAISE EXCEPTION TYPE zcx_ca_fill_bapix
        EXPORTING
          textid         = zcx_ca_fill_bapix=>parameter_wrong_type
          parameter_name = 'BAPI_DATAX'.
    ELSEIF bapi_data_typedescr->kind NE bapi_datax_typedescr->kind.
      RAISE EXCEPTION TYPE zcx_ca_fill_bapix
        EXPORTING
          textid            = zcx_ca_fill_bapix=>different_types
          parameter_name_01 = 'BAPI_DATA'
          parameter_name_02 = 'BAPI_DATAX'.
    ENDIF.

    IF bapi_data_typedescr->kind EQ cl_abap_typedescr=>kind_struct.
      bapi_data_structdescr ?= bapi_data_typedescr.
      bapi_datax_structdescr ?= bapi_datax_typedescr.
    ELSE.
      bapi_data_tabledescr ?= bapi_data_typedescr.
      bapi_datax_tabledescr ?= bapi_datax_typedescr.
      TRY.
          bapi_data_structdescr ?= bapi_data_tabledescr->get_table_line_type( ).
        CATCH cx_sy_move_cast_error.
          RAISE EXCEPTION TYPE zcx_ca_fill_bapix
            EXPORTING
              textid         = zcx_ca_fill_bapix=>parameter_wrong_type
              parameter_name = 'BAPI_DATA'.
      ENDTRY.
      TRY.
          bapi_datax_structdescr ?= bapi_datax_tabledescr->get_table_line_type( ).
        CATCH cx_sy_move_cast_error.
          RAISE EXCEPTION TYPE zcx_ca_fill_bapix
            EXPORTING
              textid         = zcx_ca_fill_bapix=>parameter_wrong_type
              parameter_name = 'BAPI_DATAX'.
      ENDTRY.
    ENDIF.
    IF _is_flat( bapi_data_structdescr ) EQ abap_false.
      RAISE EXCEPTION TYPE zcx_ca_fill_bapix
        EXPORTING
          textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
          parameter_name = 'BAPI_DATA'.
    ENDIF.
    IF _is_flat( bapi_datax_structdescr ) EQ abap_false.
      RAISE EXCEPTION TYPE zcx_ca_fill_bapix
        EXPORTING
          textid         = zcx_ca_fill_bapix=>deep_structure_not_allowed
          parameter_name = 'BAPI_DATAX'.
    ENDIF.
    parameter_kind = bapi_data_typedescr->kind.
  ENDMETHOD.


  METHOD _is_flat.
    DATA component TYPE REF TO abap_componentdescr.

    DATA components TYPE abap_component_tab.

    DATA structdescr TYPE REF TO cl_abap_structdescr.

    IF abap_structdescr IS BOUND.
      is_flat = abap_true.
      components = abap_structdescr->get_components( ).
      LOOP AT components REFERENCE INTO component.
        IF component->as_include EQ abap_true.
          structdescr ?= component->type.
          is_flat = _is_flat( structdescr ).
        ELSEIF component->type->kind NE cl_abap_typedescr=>kind_elem.
          CLEAR is_flat.
        ENDIF.
        IF is_flat EQ abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
