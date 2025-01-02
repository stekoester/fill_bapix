CLASS zcx_ca_fill_bapix DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    CONSTANTS:
      "! <p class="shorttext synchronized">General error.</p>
      "!
      BEGIN OF zcx_ca_fill_bapix,
        msgid TYPE symsgid VALUE 'ZCX_CA_FILL_BAPIX',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_fill_bapix.

    CONSTANTS:
      "! <p class="shorttext synchronized">Parameter has the wrong (only structure and table allowed).</p>
      "!
      BEGIN OF parameter_wrong_type,
        msgid TYPE symsgid VALUE 'ZCX_CA_FILL_BAPIX',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'PARAMETER_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF parameter_wrong_type.

    CONSTANTS:
      "! <p class="shorttext synchronized">Deep structure not allowed for parameter.</p>
      "!
      BEGIN OF deep_structure_not_allowed,
        msgid TYPE symsgid VALUE 'ZCX_CA_FILL_BAPIX',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'PARAMETER_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF deep_structure_not_allowed.

    CONSTANTS:
      "! <p class="shorttext synchronized">Deep structure not allowed for parameter.</p>
      "!
      BEGIN OF different_types,
        msgid TYPE symsgid VALUE 'ZCX_CA_FILL_BAPIX',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'PARAMETER_NAME_01',
        attr2 TYPE scx_attrname VALUE 'PARAMETER_NAME_02',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF different_types.

    "! <p class="shorttext synchronized">Constructor</p>
    "!
    "! @parameter textid         | <p class="shorttext synchronized">Key for Access to Message Text</p>
    "! @parameter previous       | <p class="shorttext synchronized">Exception Mapped to the Current Exception</p>
    "! @parameter parameter_name | <p class="shorttext synchronized">Subcomponent name</p>
    METHODS constructor
      IMPORTING
        textid            LIKE if_t100_message=>t100key DEFAULT zcx_ca_fill_bapix=>zcx_ca_fill_bapix
        previous          LIKE previous OPTIONAL
        parameter_name    TYPE seosconame OPTIONAL
        parameter_name_01 TYPE seosconame OPTIONAL
        parameter_name_02 TYPE seosconame OPTIONAL
          PREFERRED PARAMETER textid.

    "! <p class="shorttext synchronized">Subcomponent name</p>
    "!
    DATA parameter_name TYPE seosconame.
    "! <p class="shorttext synchronized">Subcomponent name</p>
    "!
    DATA parameter_name_01 TYPE seosconame.
    "! <p class="shorttext synchronized">Subcomponent name</p>
    "!
    DATA parameter_name_02 TYPE seosconame.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_ca_fill_bapix IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    CLEAR me->textid.

    me->parameter_name = parameter_name.
    me->parameter_name_01 = parameter_name_01.
    me->parameter_name_02 = parameter_name_02.

    if_t100_message~t100key = textid.
  ENDMETHOD.
ENDCLASS.
