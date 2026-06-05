
#' Create source diagrams for the AIGRA multimodal template
#'
#' @param diagram_dir Directory where source diagram PNG files should be saved.
#'
#' @return Named character vector of diagram paths.
#' @export
aigra_create_sample_source_diagrams <- function(diagram_dir) {
  dir.create(diagram_dir, recursive = TRUE, showWarnings = FALSE)

  diag001_path <- file.path(diagram_dir, 'diag001_velocity_time.png')
  diag002_path <- file.path(diagram_dir, 'diag002_series_circuit.png')
  diag003_path <- file.path(diagram_dir, 'diag003_inclined_plane.png')
  diag004_path <- file.path(diagram_dir, 'diag004_standing_wave.png')
  diag005_path <- file.path(diagram_dir, 'diag005_convex_lens.png')

  png(diag001_path, width = 1000, height = 800)
  plot(
    c(0, 5), c(0, 20),
    type = 'l', lwd = 4,
    xlab = 'Time (s)',
    ylab = 'Velocity (m/s)',
    main = 'Velocity-time graph',
    xlim = c(0, 6),
    ylim = c(0, 25)
  )
  points(c(0, 5), c(0, 20), pch = 19, cex = 1.4)
  grid()
  text(5, 20, '  (5 s, 20 m/s)', pos = 4)
  dev.off()

  png(diag002_path, width = 1000, height = 800)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 8))
  title('Series circuit')
  segments(2, 6, 8, 6, lwd = 3)
  segments(8, 6, 8, 2, lwd = 3)
  segments(8, 2, 2, 2, lwd = 3)
  segments(2, 2, 2, 6, lwd = 3)
  rect(3, 5.5, 4.5, 6.5, lwd = 3)
  text(3.75, 6.9, '3 \u03A9')
  rect(5.5, 5.5, 7, 6.5, lwd = 3)
  text(6.25, 6.9, '6 \u03A9')
  segments(4.6, 2, 4.6, 1.4, lwd = 3)
  segments(5.0, 2, 5.0, 1.1, lwd = 3)
  text(4.8, 0.7, '12 V battery')
  text(5, 3.5, 'closed circuit')
  dev.off()

  png(diag003_path, width = 1000, height = 800)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 8))
  title('Inclined plane with forces')
  segments(1, 2, 8, 5.5, lwd = 3)
  segments(1, 2, 8, 2, lwd = 3)
  segments(8, 2, 8, 5.5, lwd = 3)
  polygon(
    x = c(4.0, 5.2, 4.7, 3.5),
    y = c(3.5, 4.1, 5.0, 4.4),
    border = 'black',
    lwd = 3,
    col = 'white'
  )
  text(4.35, 4.25, '4 kg')
  text(2.2, 2.3, '30\u00B0')
  arrows(4.2, 4.2, 4.2, 2.6, length = 0.15, lwd = 3)
  text(4.5, 3.2, 'mg')
  arrows(4.6, 4.4, 5.4, 5.5, length = 0.15, lwd = 3)
  text(5.65, 5.6, 'N')
  arrows(4.5, 4.1, 3.5, 3.6, length = 0.15, lwd = 2)
  text(3.1, 3.5, 'mg sin 30\u00B0')
  dev.off()

  png(diag004_path, width = 1000, height = 800)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(-2.8, 2.8))
  title('Standing wave on a string')
  segments(1, 0, 9, 0, lwd = 2)
  segments(1, -0.35, 1, 0.35, lwd = 4)
  segments(9, -0.35, 9, 0.35, lwd = 4)
  x1 <- seq(1, 5, length.out = 200)
  y1 <- 1.2 * sin(pi * (x1 - 1) / 4)
  x2 <- seq(5, 9, length.out = 200)
  y2 <- -1.2 * sin(pi * (x2 - 5) / 4)
  lines(x1, y1, lwd = 4)
  lines(x2, y2, lwd = 4)
  points(c(1, 5, 9), c(0, 0, 0), pch = 19, cex = 1.2)
  text(1, -0.6, 'Node')
  text(5, -0.6, 'Node')
  text(9, -0.6, 'Node')
  text(3, 1.55, 'Antinode')
  text(7, -1.55, 'Antinode')
  arrows(1, -2.2, 9, -2.2, code = 3, angle = 90, length = 0.08, lwd = 3)
  text(5, -2.45, '2 m')
  dev.off()

  png(diag005_path, width = 1000, height = 800)
  plot.new()
  plot.window(xlim = c(-6, 6), ylim = c(-4, 4))
  title('Convex lens ray diagram')
  segments(-5.5, 0, 5.5, 0, lwd = 2)
  segments(0, -3, 0, 3, lwd = 3)
  lines(c(0, -0.35, 0.35, 0), c(-3, 0, 0, 3), lwd = 2)
  text(0.45, 3.2, 'Convex lens')
  points(c(-4, -2, 2, 4), c(0, 0, 0, 0), pch = 19)
  text(-2, -0.35, 'F')
  text(-4, -0.35, '2F')
  text(2, -0.35, 'F')
  text(4, -0.35, '2F')
  arrows(-3, 0, -3, 1.7, length = 0.15, lwd = 3)
  text(-3.25, 1.95, 'Object')
  segments(-3, 1.7, 0, 1.7, lwd = 2)
  segments(0, 1.7, 4.8, -2.4, lwd = 2)
  arrows(4.2, -1.9, 4.8, -2.4, length = 0.12, lwd = 2)
  segments(-3, 1.7, 4.8, -2.7, lwd = 2)
  arrows(4.2, -2.35, 4.8, -2.7, length = 0.12, lwd = 2)
  arrows(4.2, 0, 4.2, -2.35, length = 0.15, lwd = 3)
  text(4.55, -2.5, 'Image')
  dev.off()

  paths <- c(
    graph = diag001_path,
    circuit = diag002_path,
    mechanics = diag003_path,
    wave = diag004_path,
    optics = diag005_path
  )

  normalizePath(paths, winslash = '/', mustWork = TRUE)
}


#' Create AIGRA multimodal template items
#'
#' @param diagram_dir Directory where source diagrams are stored or should be created.
#' @param create_diagrams If TRUE, creates sample source diagram PNG files.
#'
#' @return A data frame containing multimodal sample items.
#' @export
aigra_multimodal_template_items <- function(
  diagram_dir = NULL,
  create_diagrams = TRUE
) {
  if (is.null(diagram_dir)) {
    if (!is.null(.aigra_env$backend_path)) {
      diagram_dir <- file.path(.aigra_env$backend_path, 'data', 'diagrams')
    } else {
      diagram_dir <- file.path(tempdir(), 'aigra_diagrams')
    }
  }

  if (create_diagrams) {
    paths <- aigra_create_sample_source_diagrams(diagram_dir)
  } else {
    paths <- c(
      graph = '',
      circuit = '',
      mechanics = '',
      wave = '',
      optics = ''
    )
  }

  data.frame(
    item_id = c('DIAG001', 'DIAG002', 'DIAG003', 'DIAG004', 'DIAG005'),

    stem = c(
      'The velocity-time graph below shows the motion of a car. What is the acceleration of the car?',
      'The circuit diagram below shows a 12 V battery connected in series with two resistors of 3 ohms and 6 ohms. What is the current in the circuit?',
      'A block of mass 4 kg rests on a smooth inclined plane at an angle of 30\u00B0 to the horizontal. The diagram below shows the forces acting on the block. What is the component of the block\u2019s weight parallel to the plane?',
      'The diagram below shows a standing wave formed on a stretched string fixed at both ends. What is the wavelength of the wave?',
      'The ray diagram below shows an object placed between F and 2F in front of a convex lens. What is the nature of the image formed?'
    ),

    option_A = c(
      '2 m/s^2',
      '0.50 A',
      '9.8 N',
      '0.5 m',
      'Virtual, upright, and diminished'
    ),

    option_B = c(
      '4 m/s^2',
      '1.33 A',
      '19.6 N',
      '1.0 m',
      'Real, upright, and magnified'
    ),

    option_C = c(
      '5 m/s^2',
      '2.00 A',
      '33.9 N',
      '2.0 m',
      'Real, inverted, and magnified'
    ),

    option_D = c(
      '10 m/s^2',
      '4.00 A',
      '39.2 N',
      '4.0 m',
      'Virtual, inverted, and magnified'
    ),

    correct_answer = c('B', 'B', 'B', 'C', 'C'),
    difficulty = c('B', 'B', 'B', 'B', 'B'),
    grade = c('9', '10', '10', '11', '10'),
    section = c('Mechanics', 'Electricity', 'Mechanics', 'Waves', 'Optics'),

    topic = c(
      'Velocity-time graph',
      'Series circuit',
      'Forces on an inclined plane',
      'Standing waves',
      'Convex lens'
    ),

    objective = c(
      'Calculate acceleration from a velocity-time graph.',
      'Apply Ohm\u2019s law to a simple series circuit.',
      'Resolve weight into components on an inclined plane.',
      'Determine wavelength from a standing-wave pattern.',
      'Describe image formation by a convex lens using a ray diagram.'
    ),

    source_language = rep('English', 5),
    subject = rep('Physics', 5),
    exam = rep('AIGRA Sample Multimodal Diagram Bank', 5),

    source_diagram_required = rep(TRUE, 5),
    source_diagram_path = unname(paths),
    source_diagram_caption = rep('', 5),
    source_diagram_type = c('graph', 'circuit', 'mechanics', 'wave', 'optics'),

    stringsAsFactors = FALSE
  )
}


#' Write an AIGRA multimodal Excel template
#'
#' Creates source diagram PNGs and writes a ready-to-run Excel item bank.
#'
#' @param file Output Excel file path.
#' @param diagram_dir Directory for source diagram PNGs.
#' @param overwrite If TRUE, overwrite the Excel file.
#'
#' @return Path to the Excel file.
#' @export
aigra_write_multimodal_template_excel <- function(
  file = NULL,
  diagram_dir = NULL,
  overwrite = FALSE
) {
  if (is.null(.aigra_env$backend_path)) {
    aigra_use_backend()
  }

  if (is.null(file)) {
    file <- file.path(
      .aigra_env$backend_path,
      'data',
      'aigra_sample_multimodal_items.xlsx'
    )
  }

  if (is.null(diagram_dir)) {
    diagram_dir <- file.path(.aigra_env$backend_path, 'data', 'diagrams')
  }

  if (file.exists(file) && !overwrite) {
    stop('File already exists. Use overwrite = TRUE to replace it.')
  }

  if (!requireNamespace('writexl', quietly = TRUE)) {
    stop(
      "Package 'writexl' is required. Install it with install.packages('writexl')."
    )
  }

  items <- aigra_multimodal_template_items(
    diagram_dir = diagram_dir,
    create_diagrams = TRUE
  )

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  writexl::write_xlsx(items, file)

  normalizePath(file, winslash = '/', mustWork = TRUE)
}

