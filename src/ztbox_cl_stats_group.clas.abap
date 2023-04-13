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
  methods COUNT_NOT_INITIAL
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
  methods ARE_NORMAL
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
  methods COEFFICIENT_VARIATION
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods DISPERSION_INDEX
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods QUADRATIC_MEAN
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
  methods MAD_MEAN
    importing
      !COL type NAME_FELD default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods MAD_MEDIAN
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
  methods RANGE
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
  methods COVARIANCE
    importing
      !COLS type STRING default `TABLE_LINE`
    exporting
      !E_RESULT type ANY TABLE
    returning
      value(R) type TY_AGGREGATION_T
    raising
      resumable(ZCX_TBOX_STATS) .
  methods CORRELATION
    importing
      !COLS type STRING default `TABLE_LINE`
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
private section.

  types:
    BEGIN OF ty_stats,
        group_by TYPE TABLE OF ty_group WITH DEFAULT KEY,
        stats    TYPE REF TO ztbox_cl_stats,
      END OF ty_stats .
  types:
    ty_stats_t TYPE TABLE OF ty_stats .

  data _DATA type ref to DATA .
  data _TEMP type ref to DATA .
  data _GROUPING type STRING_TABLE .
  data _GROUP_STATS type TY_STATS_T .

  methods _GET_GROUP_VALUE
    importing
      !ROW type ANY
    returning
      value(R) type TY_GROUP_T .
  methods _GROUP_TO_TAB
    importing
      !AGG type TY_AGGREGATION_T
      !COL type NAME_FELD
    exporting
      !TAB type ANY TABLE
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _PREPARE_STATS
    raising
      resumable(ZCX_TBOX_STATS) .
  methods _NEXT_FREE_COL
    importing
      !TAB type ANY TABLE
    returning
      value(R) type NAME_FELD .
ENDCLASS.



CLASS ZTBOX_CL_STATS_GROUP IMPLEMENTATION.


  METHOD ARE_NORMAL.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->are_normal( col = col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD COEFFICIENT_VARIATION.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->coefficient_variation( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


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


  METHOD CORRELATION.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->correlation( cols ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD count.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->count( ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD COUNT_NOT_INITIAL.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->count_not_initial( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD COVARIANCE.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->covariance( cols ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD DISPERSION_INDEX.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->dispersion_index( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD GEOMETRIC_MEAN.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->geometric_mean( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD MAD_MEAN.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->mad_mean( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD mad_median.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->mad_median( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD QUADRATIC_MEAN.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->quadratic_mean( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
        IMPORTING
          tab = e_result ).
    ENDIF.

  ENDMETHOD.


  METHOD range.

    r = VALUE #( FOR group IN _group_stats
      ( group_by  = group-group_by
        value     = group-stats->range( col ) ) ).

    IF e_result IS SUPPLIED.
      _group_to_tab(
        EXPORTING
          agg = r
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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
          col = _next_free_col( e_result )
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


  METHOD _next_free_col.

    DATA(tab_desc)  = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( tab ) ).
    DATA(line_desc) = CAST cl_abap_structdescr( tab_desc->get_table_line_type( ) ).

    LOOP AT line_desc->get_included_view( ) INTO DATA(comp).

      CHECK NOT line_exists( _grouping[ table_line = comp-name ] ).

      r = comp-name.
      RETURN.

    ENDLOOP.

  ENDMETHOD.


  METHOD _prepare_stats.

    TYPES: BEGIN OF ty_values,
             group_by TYPE ty_group_t,
             data     TYPE REF TO data,
           END OF ty_values.

    DATA groups TYPE TABLE OF ty_values WITH DEFAULT KEY.
    DATA group  LIKE LINE OF groups.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <grp> TYPE STANDARD TABLE.
    ASSIGN _data->* TO <tab>.

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

  ENDMETHOD.
ENDCLASS.
