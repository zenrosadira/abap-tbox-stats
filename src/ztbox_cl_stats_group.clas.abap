class ZTBOX_CL_STATS_GROUP definition
  public
  final
  create private

  global friends ZTBOX_CL_STATS .

public section.

  types:
    BEGIN OF ty_group,
        group_field TYPE name_feld,
        group_value TYPE string,
      END OF ty_group .
  types:
    ty_group_t TYPE TABLE OF ty_group WITH DEFAULT KEY .
  types:
    BEGIN OF ty_aggregation,
        group_by TYPE TABLE OF ty_group WITH DEFAULT KEY,
        value    TYPE string,
      END OF ty_aggregation .
  types:
    ty_aggregation_t TYPE TABLE OF ty_aggregation WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !DATA type ref to DATA
      !GROUPING type STRING_TABLE
    raising
      resumable(ZCX_TBOX_STATS) .
  methods COUNT
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods COUNT_DISTINCT
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods KURTOSIS
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods SKEWNESS
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods STANDARD_DEVIATION
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods VARIANCE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MEDIAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods INTERQUARTILE_RANGE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods THIRD_QUARTILE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods SECOND_QUARTILE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods FIRST_QUARTILE
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods GEOMETRIC_MEAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods HARMONIC_MEAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MEAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods SUM
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MAX
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MIN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods COL
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    returning
      value(R) type ref to ZTBOX_CL_STATS_GROUP
    raising
      resumable(ZCX_TBOX_STATS) .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_stats,
        group_by TYPE TABLE OF ty_group WITH DEFAULT KEY,
        stats    TYPE REF TO ztbox_cl_stats,
      END OF ty_stats .
    TYPES:
      ty_stats_t TYPE TABLE OF ty_stats .

    DATA _data TYPE REF TO data .
    DATA _temp TYPE REF TO data .
    DATA _grouping TYPE string_table .
    DATA _group_stats TYPE ty_stats_t .

    METHODS _prepare .
    METHODS _get_group_value
      IMPORTING
        !row     TYPE any
      RETURNING
        VALUE(r) TYPE ty_group_t .
    METHODS _group_to_tab
      IMPORTING
        !agg TYPE ty_aggregation_t
        !col TYPE name_feld
      EXPORTING
        !tab TYPE ANY TABLE
      RAISING
        RESUMABLE(zcx_tbox_stats) .
    METHODS _prepare_stats
      RAISING
        RESUMABLE(zcx_tbox_stats).
ENDCLASS.



CLASS ZTBOX_CL_STATS_GROUP IMPLEMENTATION.


  METHOD col.

    r = NEW #( grouping = _grouping data = _data ).

    LOOP AT r->_group_stats ASSIGNING FIELD-SYMBOL(<group>).

      <group>-stats = <group>-stats->col( col ).

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    _data     = data.
    _grouping = VALUE #( FOR _grp IN grouping ( condense( _grp ) ) ).

    _prepare_stats( ).

  ENDMETHOD.


  METHOD count.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->count( ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `COUNT`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD count_distinct.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->count_distinct( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `COUNT`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD first_quartile.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->first_quartile( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `FIRST_QUARTILE`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD geometric_mean.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->geometric_mean( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `GEOMETRIC_MEAN`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD harmonic_mean.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->harmonic_mean( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `HARMONIC_MEAN`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD interquartile_range.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->interquartile_range( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `INTERQUARTILE_RANGE`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD kurtosis.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->kurtosis( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `KURTOSIS`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD max.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->max( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = col
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD mean.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->mean( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = col
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD median.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->median( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `MEDIAN`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD min.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->min( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = col
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD second_quartile.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->second_quartile( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `SECOND_QUARTILE`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD skewness.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->skewness( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `SKEWNESS`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD standard_deviation.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->standard_deviation( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `STANDARD_DEVIATION`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD sum.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->sum( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = col
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD third_quartile.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->third_quartile( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `THIRD_QUARTILE`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD variance.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->variance( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = `VARIANCE`
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD _get_group_value.

    LOOP AT _grouping INTO DATA(group_field).

      ASSIGN COMPONENT group_field OF STRUCTURE row TO FIELD-SYMBOL(<val>).

      APPEND VALUE #(
        group_field = group_field
        group_value = <val> ) TO r.

    ENDLOOP.

  ENDMETHOD.


  METHOD _group_to_tab.

    CLEAR tab.

    DATA row TYPE REF TO data.
    CREATE DATA row LIKE LINE OF tab.
    ASSIGN row->* TO FIELD-SYMBOL(<row>).

    LOOP AT agg INTO DATA(row_agg).

      CLEAR <row>.

      LOOP AT row_agg-group_by INTO DATA(group).
        ASSIGN COMPONENT group-group_field OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
        <val> = group-group_value.
      ENDLOOP.

      ASSIGN COMPONENT col OF STRUCTURE <row> TO <val>.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_tbox_stats EXPORTING textid = zcx_tbox_stats=>column_group_not_found column = col.
      ENDIF.
      <val> = row_agg-value.

      INSERT <row> INTO TABLE tab.

    ENDLOOP.

  ENDMETHOD.


  METHOD _prepare.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    ASSIGN _data->* TO <tab>.

    CLEAR _temp.
    CREATE DATA _temp LIKE <tab>.

    FIELD-SYMBOLS <temp> TYPE STANDARD TABLE.
    ASSIGN _temp->* TO <temp>.

    <temp> = <tab>.

  ENDMETHOD.


  METHOD _prepare_stats.

    TYPES: BEGIN OF ty_values,
             group_by TYPE ty_group_t,
             data     TYPE REF TO data,
           END OF ty_values.

    DATA groups TYPE TABLE OF ty_values WITH DEFAULT KEY.
    DATA group  LIKE LINE OF groups.

    _prepare( ).

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <grp> TYPE STANDARD TABLE.
    ASSIGN _temp->* TO <tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).

      DATA(group_values) = _get_group_value( <row> ).

      IF <grp> IS ASSIGNED.
        UNASSIGN <grp>.
      ENDIF.

      READ TABLE groups WITH KEY group_by = group_values ASSIGNING FIELD-SYMBOL(<group>).

      IF sy-subrc EQ 0.

        ASSIGN <group>-data->* TO <grp>.
        INSERT <row> INTO TABLE <grp>.

      ELSE.

        CREATE DATA group-data LIKE <tab>.
        ASSIGN group-data->* TO <grp>.
        INSERT <row> INTO TABLE <grp>.
        group-group_by = group_values.
        APPEND group TO groups.

      ENDIF.

    ENDLOOP.

    LOOP AT groups INTO group.

      ASSIGN group-data->* TO <grp>.

      APPEND VALUE #(
        group_by  = group-group_by
        stats     = NEW #( <grp> )
        ) TO _group_stats.

    ENDLOOP.

    CLEAR _temp.

  ENDMETHOD.
ENDCLASS.
