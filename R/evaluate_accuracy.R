#' Helper function to evaluate prediction accuracy
#'
#' Currently, this function only works for K protocol data
#'
#' @param flashes a list containing flash-level information. Should have
#' TrialNR, StimulusType, and StimulusCode.
#' @param score prediction scores for each flash.
#' @param cutoff for flash-level accuracy only.
#' A flash with score > cutoff will be predicted as target,
#' @returns The flash-level accuracy and character-level accuracy
#'
#' @importFrom dplyr %>% as_tibble group_by mutate row_number summarize ungroup
#' @importFrom tibble add_column
#' @export

evaluate_accuracy = function(flashes, score, cutoff = 0.5) {
  flashes %>%
    as_tibble() %>%
    group_by(TrialNR) %>%
    mutate(SequenceNR = (row_number() - 1) %/% 12) %>%
    add_column(Score = score) %>%
    ungroup() ->
    tb
  # Step 1
  tb %>%
    group_by(TrialNR, StimulusCode) %>%
    summarize(StimulusType = unique(StimulusType),
              Score = mean(Score)) %>%
    ungroup() ->
    tb
  # Step 2
  tb %>%
    group_by(TrialNR, StimulusCode) %>%
    summarize(StimulusType = unique(StimulusType),
              Score = mean(Score)) %>%
    ungroup() ->
    tb
  # Step 3
  tb %>%
    mutate(Type = ifelse(StimulusCode <= 6, 'r', 'c'),
           .before = 3) %>%
    group_by(TrialNR, Type) %>%
    summarize(TrueCode = StimulusCode[StimulusType == 1],
              PredictedCode = StimulusCode[which.max(Score)]) ->
    tb
  # Step 4
  tb %>%
    group_by(TrialNR) %>%
    summarize(correct = all(TrueCode == PredictedCode)) ->
    tb

  return(list(
    flash = mean(flashes$StimulusType == (score > cutoff)),
    character = mean(tb$correct)
  ))
}
