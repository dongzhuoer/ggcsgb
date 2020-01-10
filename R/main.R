
filter_chrono_strati_chart <- function(chart, range, start, entire, atom, offset_first, offset_last) {
	flatten <- function(df) {
		result <- df[1, ]
		result$begin = df[nrow(df), 'begin'];
		result;
	}

	entire_position <- which(colnames(chart) == entire);
	
	#" some Epoch of different Period have the same name, such as 'Upper'
	label_data <- chart %>% 
		dplyr::mutate(Epoch = paste0(Period, '/', 'Epoch')) %>%
		dplyr::select(1:entire_position, begin, end) %>%
		plyr::ddply(entire, flatten) %>% 
		{dplyr::as.tbl(.[order(.$begin), ])} %>%
		dplyr::mutate(Epoch = stringr::str_replace(Period, '\\w+/', ''));
	
	first <- dplyr::filter(label_data, end < start - range[1]) %>% nrow;
	last <- dplyr::filter(label_data, begin <= start - range[2]) %>% nrow + 1;
	
	label_data = label_data[(last - offset_last):(first + offset_first), ];
	
	color_data <- dplyr::select(chart, 'begin', 'age_color', 'end') %>% 
		dplyr::filter(begin <= label_data$begin[nrow(label_data)], end >= label_data$end[1]);

	list(color_data = color_data, title_data = label_data);
}

#' @title add chronostratigraphic bar to annotate an ggtree plot
#' 
#' @details Basic ggtree consists of a layer of horizontal lines and a layer of vertical lines, so `max(ggplot2::ggplot_build(plot)$data[[1]]$xend)` gives the time when the MRCA comes into being (previous `start` parameter).
#' 
#' @param offset_first integer scalar. Add how many `entire` at the begining.
#' @param offset_last integer scalar. Add how many `entire` at the end.
#' @param entire string. One of `'Era'`, `'Period'`, `'Epoch'`, `'Age'`. The time unit to remain entire, since you have to keep it integrity if you want to add a label for that time unit.
#' @param atom string. Minimum time unit, only support `'Age'` now.
#' @param chart data.frame. The 
#'
#' @return ggplot object chrono_strati_chart data to use, you usually needn't care it.
#' @export
chrono_strati_arg <- function(start, offset_first = 0, offset_last = 0, 
							  entire = 'Period', atom = 'Age', 
							  chart = ggcsgb::Phanerozoic) {
	if (!identical(atom, 'Age')) stop('atom must be "Age"');

	entires <- c('Era', 'Period', 'Epoch', 'Age');
	if (length(entire) != 1L) stop('entire must be of length 1');
	if (!(entire %in% entires)) stop(paste('entire must be one of', deparse(entires)));

	structure(list(chart = chart, entire = entire, atom = atom, 
				   offset_first = offset_first, offset_last = offset_last), 
			  class = 'chrono_strati_arg');
}


#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.chrono_strati_arg <- function(object, plot, object_name) {
    object$start = max(ggplot2::ggplot_build(plot)$data[[1]]$xend)
	#" only for internal node, no tip
	object$range <- plot$data %>% dplyr::filter(!isTip) %>% {range(.$x)};

	attr(plot, 'chrono_strati') <- c(object, do.call(filter_chrono_strati_chart, object));
	attr(plot, 'chrono_strati')$n_taxa = plot$data %>% dplyr::filter(isTip) %>% nrow;
	plot;
}



#' @title plot chronostratigraphic bar
#' @param plot ggplot2 object.
#' @param data data.frame. contains `color`, `begin`, `end`
#' @param start numeric scalar. The start of time, Mya.
#' @param height numeric scalar. 
#'
#' @return ggplot2 object
draw_chrono_strati_bar <- function(plot, data, start, height) {
	#" now the meaning of time changes
	data %<>% dplyr::mutate(begin = start - begin, end = start - end, 
							color = paste0('#', age_color));

	n_layer <- length(plot$layers);
	
	for (i in 1:nrow(data)) {
		chrono_strati_bar <- 
			ggplot2::annotate("rect", fill = data$color[i], ymin = 0.5, ymax = height + 0.5, 
							  xmin = data$end[i], xmax = data$begin[i]);
		plot = plot + chrono_strati_bar;
	}
	
	#" put annotate layers underneath existing layers
	layer_index <- c((n_layer + 1):(n_layer + nrow(data)), 1:n_layer);
	plot$layers = plot$layers[layer_index];
	
	plot;
}

#' @title add chronostrati bars to a plot
#'
#' @description add chronostrati bars implemented by [ggplot2::annotate("rect")][ggplot2::annotate] to a ggplot object
#' @export
chrono_strati_bar <- function(...) {
	structure(list(...), class = 'chrono_strati_bar')
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.chrono_strati_bar <- function(object, plot, object_name) {
	draw_chrono_strati_bar(
		plot, 
		attr(plot, 'chrono_strati')$color_data, 
		attr(plot, 'chrono_strati')$start,
		attr(plot, 'chrono_strati')$n_taxa
	)
}


#' @param ... other arguments passed on to [ggplot2::geom_text()]
#' @noRd
geom_chrono_strati_axis <- function(data, start, y, trim_left, squash, present, ...) {
#plot=ggplot2::ggplot();data = Phanerozoic;start=336;y=30;trim_left=T;squash = 0.015;present=T
	tick <- data %>% as.data.frame %>% {c(.$begin, .$end)} %>% unique %>% sort;
	if (trim_left) tick %<>% {.[. <= start]};
	if (squash > 0 && length(tick) > 1L) {
		d_time <- tick %>% {max(.) - min(.)} * squash;
		tick2 <- tick[length(tick)];
		
		for (i in tick[length(tick):2]) 
			if (tick2[1] - i >= d_time) tick2 %<>% append(i, .);
		
		tick = c(tick[1], tick2[-1]) # the first and last time are always preserved
	}
	
	df <- tibble::data_frame(x = start - tick, y = y, label = formatC(tick, 1, format = 'f'))
	if (present && df$label[1] == '0.0') df$label[1] = 'Present';

	ggplot2::geom_text(ggplot2::aes(x, y, label = label), df, ...)
}
	

#' @title add axis text
#' 
#' @param offset numeric scalar. How much is the axis text higher than bar.
#' @param trim_left logical scalar. Whether to drop axis text which is before the MRCA
#' @param squash numeric scalar. Whether to remove some axis text if they are too close. If `0`, don't remove; else if the interval of two text is less than `squash*whole_axis_length`, one of the two is removed.
#' @param present numeric scalar. Whether to replace `0` with `present`
#' @param angle,size,hjust,... other arguments passed on to [ggplot2::geom_text()]
#'
#' @export
chrono_strati_axis <- 
	function(offset = 1, trim_left = T, squash = 0.015, present = T, angle = 90, 
			 size = 3, hjust = 0, ...) {
	structure(
		list(offset = offset, trim_left = trim_left, squash = squash, 
			 present = present, angle = angle, size = size, hjust = hjust, ...), 
		class = 'chrono_strati_axis'
	)
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.chrono_strati_axis <- function(object, plot, object_name) {
	object$data = attr(plot, 'chrono_strati')$color_data;
	object$start = attr(plot, 'chrono_strati')$start;
	object$y = attr(plot, 'chrono_strati')$n_taxa + object$offset;
	object$offset <- NULL;
	
	plot + do.call(geom_chrono_strati_axis, object);
}



draw_chrono_strati_label <- function(plot, data, start, y, entire, tick_length, label_offset, line_params, ...) {
	Start <- start;
	tick <- Start - data %>% as.data.frame %>% {c(.$begin, .$end)} %>% unique %>% sort;
	tick_data <- tibble::data_frame(x = tick, ymin = y - tick_length, ymax = y) %>% 
		reshape2::melt(id = 'x', value.name = 'y');
	
	geom_args <- line_params;
	geom_args$mapping = ggplot2::aes(x,y, group = x);
	geom_args$data = tick_data;
	
	annotate_args <- line_params;
	annotate_args$geom = 'segment';
	annotate_args$x = tick[1];
	annotate_args$xend = tick[length(tick)];
	annotate_args$y = y;
	annotate_args$yend = y;
	
	plot + 
		do.call(ggplot2::geom_line, geom_args) + 
		do.call(ggplot2::annotate, annotate_args) +
		ggplot2::geom_text(ggplot2::aes_string('x', 'y', label = entire), 
			  data %>% dplyr::mutate(x = Start - (begin + end)/2, y = y + label_offset),
			  ...)
}
#

#' @title add label and tick
#' 
#' @param offset numeric scalar. How much is the axis higher than bar.
#' @param tick_length numeric scalar. How long is the tick.
#' @param label_offset numeric scalar.  How much is the label higher than the axis.
#' @param line_params other arguments passed on to [ggplot2::geom_line()] and [ggplot2::annotate('segment')][ggplot2::annotate]
#' @param size,... other arguments passed on to [ggplot2::geom_text()]
#'
#' @export
chrono_strati_label <- function(offset = 6, tick_length = 1, label_offset = 2, line_params = list(), size=3.5, ...) {
	structure(
		list(offset = offset, tick_length = tick_length, label_offset = label_offset, 
			 line_params = line_params, size = size, ...), 
		class = 'chrono_strati_label'
	);
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.chrono_strati_label <- function(object, plot, object_name) {
	object$plot = plot;
	object$data = attr(plot, 'chrono_strati')$title_data;
	object$start = attr(plot, 'chrono_strati')$start;
	object$y = attr(plot, 'chrono_strati')$n_taxa + object$offset;
	object$offset <- NULL;
	object$entire = attr(plot, 'chrono_strati')$entire;
	
	do.call(draw_chrono_strati_label, object);
}



#' plot = ggcsgb::Wang2017_tree;
#' start = 333;

    # Wang2017_tree %>% ggcsgb::add_chrono_strati_bar(333, 1);
    # Wang2017_tree %>% ggcsgb::add_chrono_strati_bar(333, entire = 'Era');
    # Wang2017_tree %>% ggcsgb::add_chrono_strati_bar(333, entire = 'Period');
    # Wang2017_tree %>% ggcsgb::add_chrono_strati_bar(333, entire = 'Epoch');
    # Wang2017_tree %>% ggcsgb::add_chrono_strati_bar(333, entire = 'Age'); # to do
    # 
    # Prum2015_tree %>% ggcsgb::add_chrono_strati_bar(64.5, entire = 'Epoch')
