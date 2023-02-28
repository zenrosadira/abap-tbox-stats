class ZCX_TBOX_STATS definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_TBOX_STATS,
      msgid type symsgid value '00',
      msgno type symsgno value '208',
      attr1 type scx_attrname value 'ERROR',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TBOX_STATS .
  data ERROR type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !ERROR type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_TBOX_STATS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->ERROR = ERROR .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_TBOX_STATS .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
