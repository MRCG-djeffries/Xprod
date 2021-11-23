  


function dat_table_module_js(ns_prefix) {
  
    $("#" + ns_prefix + "param_table").on("click", ".show_btn", function() {
    Shiny.setInputValue(ns_prefix + "dat_id_to_show", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
}

