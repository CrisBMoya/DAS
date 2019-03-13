#Landing page as a modal function.
LandingTab=function(){
  showModal(ui=modalDialog(title="Landing Page: ",
                           splitLayout(actionButton(inputId="An1", label="PLS & PCA")),
                           splitLayout(actionButton(inputId="An2", label="Differential Expression")),
                           splitLayout(actionButton(inputId="An3", label="Correlation"))
  ))
}