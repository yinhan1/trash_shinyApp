pretty_action_button <- function(id, label = '', 
                                 class = 'btn btn-success',
                                 width = '100%',
                                 disabled = '') {
  html_text <- glue::glue(
    '<button class="{class} action-button" 
             type="button" style="width: {width}" {disabled}
             id={id}>
        {label}
      </button>'
  )
  shiny::HTML(html_text)
}

pretty_download_button <- function(id, label = '', 
                                   class = 'btn btn-info', icon_class='fa fa-download',
                                   width = '100%',
                                   disabled = '') {
  html_text <- glue::glue(
    '<a id="{id}" class="shiny-download-link " 
      href="" target="_blank" download>
      <button class="{class}" type="button" style="width: {width}" {disabled}>
       <i class="{icon_class}" style="padding-right: 10px"></i>
       {label}
     </button>
   </a>'
  )
  shiny::HTML(html_text)
}

custom_text_input <- function(id, label = '', value = '',
                              style = '', disabled = '') {
  html_text <- glue::glue(
    '<div class="form-group shiny-input-container">
       <label for="{id}">{label}</label>
       <input id="{id}" type="text" class="form-control" 
              value="{value}" {disabled}
       />
     </div>'
  )
  shiny::HTML(html_text)
}

