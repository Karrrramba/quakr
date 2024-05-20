# GeomTimelineLabel throws an error with correct message when n_max is used without limit

    Code
      create_plot(sample_data)
    Condition
      Error in `geom_timeline_label()`:
      ! Problem while converting geom to grob.
      i Error occurred in the 2nd layer.
      Caused by error in `draw_group()`:
      ! `n_max` must be used in conjunction with `limit`.

