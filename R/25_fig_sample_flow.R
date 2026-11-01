# 25_fig_sample_flow: the decision-flow sankey, drawn as
# SVG in base R from the decision-flow tables. Hue = who acts (AI orange,
# human blue, neither grey); hatching = stance on the agent's non-preferred
# ("other") side; uncontested flows bypass the middle band.

source("R/_setup.R")

nodes <- read_result_table("decision_flow")
xt    <- read_result_table("decision_flow_crosstab")

nn <- function(node) {
  v <- nodes$n[nodes$node == node]
  stopifnot(length(v) == 1)
  as.integer(v)
}
cc <- function(op, stance, cont, fin) {
  v <- xt$n[xt$opener == op & xt$opening_stance == stance &
            xt$contested == cont & xt$final == fin]
  stopifnot(length(v) == 1)
  as.integer(v)
}
csum <- function(op, cont, fin = c("prior", "other"), stance = c("lean", "counter"))
  as.integer(sum(xt$n[xt$opener == op & xt$contested == cont &
                      xt$final %in% fin & xt$opening_stance %in% stance]))

# bypass strands cannot change stance by construction
stopifnot(cc("Human", "lean", "uncontested", "other") == 0,
          cc("Human", "counter", "uncontested", "prior") == 0)

v <- list(
  total    = nn("all decisions"),
  neither  = nn("nobody voices first"),
  aiFirst  = nn("agent voices first"),
  huFirst  = nn("participant voices first"),
  gAi      = nn("goes along"),
  huCon    = nn("contests"),
  gHu      = csum("Human", "uncontested"),
  aiCon    = nn("agent contests"),
  prior    = nn("lean side"),
  other    = nn("counter side"))
stopifnot(v$prior + v$other == v$total)

# ---- layout (fixed design constants; every count comes from the tables) -----
L1 <- tibble(id = c("neither", "aiFirst", "humanFirst"),
             lines = list("Neither takes a side", "AI takes a side first",
                          "Human takes a side first"),
             n = c(v$neither, v$aiFirst, v$huFirst))
MID <- tibble(id = c("gNeither", "gAi", "humanContests", "gHu", "aiContests"),
              kind = c("ghost", "tab", "tab", "ghost", "tab"),
              n = c(v$neither, v$gAi, v$huCon, v$gHu, v$aiCon),
              lines = list(NULL, c("uncontested", "AI continues"), "Human contests",
                           "uncontested", "AI contests"),
              lvl = c(0, 0, 0, 0, 1), below = c(0, 0, 0, 1, 0))
L3 <- tibble(id = c("prior", "other"),
             lines = list("Final choice: AI's hidden prior", "Final choice: the other side"),
             n = c(v$prior, v$other))
flows <- bind_rows(
  tibble(type = "BYP", from = c("neither", "humanFirst", "humanFirst"),
         via = c("gNeither", "gHu", "gHu"), to = c("prior", "prior", "other"),
         st = c("none", "lean", "counter"),
         v = c(v$neither, cc("Human", "lean", "uncontested", "prior"),
               cc("Human", "counter", "uncontested", "other"))),
  tibble(type = "CIN", from = c(rep("aiFirst", 4), rep("humanFirst", 2)), via = NA,
         to = c("gAi", "gAi", "humanContests", "humanContests", "aiContests", "aiContests"),
         st = rep(c("lean", "counter"), 3),
         v = c(csum("AI", "uncontested", stance = "lean"),
               csum("AI", "uncontested", stance = "counter"),
               csum("AI", "contested", stance = "lean"),
               csum("AI", "contested", stance = "counter"),
               csum("Human", "contested", stance = "lean"),
               csum("Human", "contested", stance = "counter"))),
  tibble(type = "COUT", from = rep(c("gAi", "humanContests", "aiContests"), each = 2),
         via = NA, to = rep(c("prior", "other"), 3), st = NA,
         v = c(csum("AI", "uncontested", fin = "prior"), csum("AI", "uncontested", fin = "other"),
               csum("AI", "contested", fin = "prior"), csum("AI", "contested", fin = "other"),
               csum("Human", "contested", fin = "prior"), csum("Human", "contested", fin = "other"))))
stopifnot(sum(flows$v[flows$type == "CIN"]) == v$aiFirst + v$aiCon,
          sum(flows$v[flows$type == "COUT"]) + sum(flows$v[flows$type == "BYP"]) == v$total)
# label anchors [x, y]; NA x = centered on the node
LPOS <- list(neither = c(113, 83), aiFirst = c(573.95, 83), humanFirst = c(1106.28, 83),
             humanContests = c(NA, 424), aiContests = c(1215, 424), gAi = c(NA, 424),
             gHu = c(NA, 489), prior = c(528.77, 928), other = c(1094.77, 928))

# number formatting: two decimals, ties rounded
# up on the exact binary value
f2 <- function(x) vapply(x, function(z) {
  s <- sprintf("%.60f", abs(z))
  ip <- sub("\\..*$", "", s); fp <- sub("^.*\\.", "", s)
  q <- as.numeric(paste0(ip, substr(fp, 1, 2))) + (as.integer(substr(fp, 3, 3)) >= 5)
  sprintf("%s%.0f.%02.0f", if (z < 0) "-" else "", q %/% 100, q %% 100)
}, character(1))
ni <- function(x) { stopifnot(x == round(x)); sprintf("%.0f", x) }
jround <- function(x) floor(x + 0.5)

build_sankey <- function(TOT, PL = 11, W = 1330, centerW = 1300, H = 1215,
                         legendY = 60, OFF = 200) {
  G <- 16; K <- 0.75; barH <- 22; RGAP <- 7; N <- 96
  Y1 <- 150 + OFF; YM <- 520 + OFF; Y3 <- 860 + OFF; B1 <- Y1 + barH; BM <- YM + barH
  FS <- 29; LH <- 37
  pc <- function(n) n / TOT * 100
  HUE <- list(or = c(tab = "#e0801f", rib = "#ECB379"),
              bl = c(tab = "#2f6fae", rib = "#82A9CE"),
              gr = c(tab = "#605d5d", rib = "#A09E9E"))
  AG <- c(neither = "gr", aiFirst = "or", humanFirst = "bl", gAi = "or",
          humanContests = "bl", aiContests = "or", prior = "gr", other = "gr")
  pats <- character(0)
  hatch <- function(col) {
    id <- paste0("h", sub("#", "", col, fixed = TRUE))
    if (!id %in% names(pats)) pats[[id]] <<- col
    paste0("url(#", id, ")")
  }
  sr <- c(none = 0, lean = 1, counter = 2); hr <- c(gr = 0, or = 1, bl = 2)

  place <- function(a) {
    x <- 0; a$w <- a$x <- a$cx <- NA_real_
    for (i in seq_len(nrow(a))) {
      a$w[i] <- pc(a$n[i]) * PL; a$x[i] <- x; a$cx[i] <- x + a$w[i] / 2
      x <- x + a$w[i] + G
    }
    a
  }
  L1 <- place(L1); MID <- place(MID); L3 <- place(L3)
  dL <- function(cx, w) cx - K * w / 2
  dR <- function(cx, w) cx + K * w / 2
  allx <- bind_rows(L1, MID, L3)
  bbL <- min(dL(allx$cx, allx$w)); bbR <- max(dR(allx$cx, allx$w))
  shift <- (centerW - (bbR - bbL)) / 2 - bbL
  L1$x <- L1$x + shift; L1$cx <- L1$cx + shift
  MID$x <- MID$x + shift; MID$cx <- MID$cx + shift
  L3$x <- L3$x + shift; L3$cx <- L3$cx + shift
  L1$top <- Y1; MID$top <- YM; L3$top <- Y3
  nd <- bind_rows(L1 %>% mutate(layer = 1), MID %>% mutate(layer = 2),
                  L3 %>% mutate(layer = 3))
  node <- function(id) nd[nd$id == id, ]
  sq <- function(nod, c) nod$cx + K * (c - nod$cx)

  fl <- flows
  for (k in c("sx", "mx", "tx")) for (s in c("l", "", "r")) fl[[paste0(k, s)]] <- NA_real_
  stack <- function(id, idx, key) {
    nod <- node(id); o <- 0
    for (i in idx) {
      w <- pc(fl$v[i]) * PL
      fl[[paste0(key, "l")]][i] <<- sq(nod, nod$x + o)
      fl[[key]][i]              <<- sq(nod, nod$x + o + w / 2)
      fl[[paste0(key, "r")]][i] <<- sq(nod, nod$x + o + w)
      o <- o + w
    }
  }
  cx_of <- function(ids) vapply(ids, function(i) node(i)$cx, numeric(1))
  segs <- list(); outs <- list(); segkey <- list()
  for (id in L1$id) {
    idx <- c(which(fl$type == "BYP" & fl$from == id), which(fl$type == "CIN" & fl$from == id))
    tox <- cx_of(ifelse(is.na(fl$via[idx]), fl$to[idx], fl$via[idx]))
    idx <- idx[order(sr[fl$st[idx]], tox, method = "radix")]
    stack(id, idx, "sx"); segs[[id]] <- idx; segkey[[id]] <- "sx"
  }
  for (j in seq_len(nrow(MID))) {
    id <- MID$id[j]
    if (MID$kind[j] == "ghost") {
      idx <- which(fl$type == "BYP" & fl$via %in% id)
      idx <- idx[order(sr[fl$st[idx]], cx_of(fl$to[idx]), method = "radix")]
      stack(id, idx, "mx")
    } else {
      idx <- which(fl$type == "CIN" & fl$to == id)
      idx <- idx[order(sr[fl$st[idx]], method = "radix")]
      stack(id, idx, "tx"); segs[[id]] <- idx; segkey[[id]] <- "tx"
      o <- which(fl$type == "COUT" & fl$from == id)
      o <- o[order(cx_of(fl$to[o]), method = "radix")]
      stack(id, o, "sx"); outs[[id]] <- o
    }
  }
  for (id in L3$id) {
    b <- which(fl$type == "BYP" & fl$to == id); co <- which(fl$type == "COUT" & fl$to == id)
    idx <- c(b, co); at <- c(fl$mx[b], fl$sx[co])
    idx <- idx[order(hr[AG[fl$from[idx]]], at, method = "radix")]
    segs[[id]] <- idx; segkey[[id]] <- "tx"; stack(id, idx, "tx")
  }

  sm <- function(t) t * t * t * (t * (t * 6 - 15) + 10)
  arc <- function(x0, y0, x1, y1) {
    t <- (1:N) / N
    paste0(" L", f2(x0 + (x1 - x0) * sm(t)), ",", f2(y0 + (y1 - y0) * t), collapse = "")
  }
  MIDY <- YM + barH / 2
  paint <- function(hue, st) if (st == "counter") hatch(HUE[[hue]][["rib"]]) else HUE[[hue]][["rib"]]
  opa <- function(st) if (st == "counter") "0.9" else "0.85"
  finalSt <- function(i) if (fl$to[i] == "prior") "lean" else "counter"
  path <- function(d, stroke, op, w)
    paste0('<path d="', d, '" stroke="', stroke, '" stroke-opacity="', op,
           '" stroke-width="', w, '"></path>')
  by_v <- function(type) { i <- which(fl$type == type); i[order(-fl$v[i], method = "radix")] }

  body <- '<g stroke-linecap="butt" fill="none">'
  for (i in by_v("BYP")) {
    y0 <- B1 + RGAP; y1 <- Y3 - RGAP; h <- AG[[fl$from[i]]]; w <- f2(pc(fl$v[i]) * PL * K)
    s1 <- fl$st[i]; s2 <- if (s1 == "none") "none" else finalSt(i)
    body <- paste0(body,
      path(paste0("M", f2(fl$sx[i]), ",", f2(y0), arc(fl$sx[i], y0, fl$mx[i], YM),
                  " L", f2(fl$mx[i]), ",", f2(MIDY)), paint(h, s1), opa(s1), w),
      path(paste0("M", f2(fl$mx[i]), ",", f2(MIDY), " L", f2(fl$mx[i]), ",", f2(BM),
                  arc(fl$mx[i], BM, fl$tx[i], y1)), paint(h, s2), opa(s2), w))
  }
  for (i in by_v("CIN")) {
    y0 <- B1 + RGAP; y1 <- YM - RGAP
    body <- paste0(body, path(paste0("M", f2(fl$sx[i]), ",", f2(y0),
                                     arc(fl$sx[i], y0, fl$tx[i], y1)),
                              paint(AG[[fl$from[i]]], fl$st[i]), opa(fl$st[i]),
                              f2(pc(fl$v[i]) * PL * K)))
  }
  for (i in by_v("COUT")) {
    y0 <- BM + RGAP; y1 <- Y3 - RGAP; s <- finalSt(i)
    body <- paste0(body, path(paste0("M", f2(fl$sx[i]), ",", f2(y0),
                                     arc(fl$sx[i], y0, fl$tx[i], y1)),
                              paint(AG[[fl$from[i]]], s), opa(s),
                              f2(pc(fl$v[i]) * PL * K)))
  }
  body <- paste0(body, "</g><g>")

  for (r in seq_len(nrow(nd))) {
    nod <- nd[r, ]; id <- nod$id
    if (!is.na(nod$kind) && nod$kind == "ghost") next
    if (!is.na(nod$kind) && nod$kind == "tab") {
      x0 <- dL(nod$cx, nod$w); x1 <- dR(nod$cx, nod$w); y0 <- nod$top; y1 <- nod$top + barH
      base <- HUE[[AG[[id]]]][["tab"]]
      lin <- segs[[id]][fl$st[segs[[id]]] == "lean"]
      pout <- outs[[id]][fl$to[outs[[id]]] == "prior"]
      inB <- if (length(lin)) fl$txr[lin[length(lin)]] else x0
      outB <- if (length(pout)) fl$sxr[pout[length(pout)]] else x0
      body <- paste0(body,
        '<polygon points="', f2(x0), ",", ni(y0), " ", f2(inB), ",", ni(y0), " ",
        f2(outB), ",", ni(y1), " ", f2(x0), ",", ni(y1), '" fill="', base, '"></polygon>',
        '<polygon points="', f2(inB), ",", ni(y0), " ", f2(x1), ",", ni(y0), " ",
        f2(x1), ",", ni(y1), " ", f2(outB), ",", ni(y1), '" fill="', hatch(base), '"></polygon>',
        '<line x1="', f2(inB), '" y1="', ni(y0), '" x2="', f2(outB), '" y2="', ni(y1),
        '" stroke="#ffffff" stroke-width="3"></line>')
      next
    }
    term <- if (id == "prior") "lean" else if (id == "other") "counter" else NA
    key <- segkey[[id]]; sg <- list()
    for (i in segs[[id]]) {
      h <- if (!is.na(AG[fl$from[i]])) AG[[fl$from[i]]] else AG[[id]]
      st <- if (!is.na(term)) term else fl$st[i]
      p <- length(sg)
      if (p && sg[[p]]$h == h && sg[[p]]$st == st) sg[[p]]$b <- fl[[paste0(key, "r")]][i]
      else sg[[p + 1]] <- list(a = fl[[paste0(key, "l")]][i], b = fl[[paste0(key, "r")]][i],
                               h = h, st = st)
    }
    for (g in sg) {
      base <- HUE[[g$h]][["tab"]]
      body <- paste0(body, '<rect x="', f2(g$a), '" y="', ni(nod$top), '" width="',
                     f2(g$b - g$a), '" height="', ni(barH), '" fill="',
                     if (g$st == "counter") hatch(base) else base, '"></rect>')
    }
  }
  body <- paste0(body, "</g>")

  val <- function(n) paste0(jround(pc(n)), "%  (n=", n, ")")
  LAB <- list()
  put <- function(id, cx, firstBaseline, lines, al = NA) {
    o <- LPOS[[id]]
    x <- if (!is.null(o) && !is.na(o[1])) o[1] else cx
    top <- if (!is.null(o)) o[2] + OFF else firstBaseline - 20
    LAB[[length(LAB) + 1]] <<- list(id = id, cx = as.numeric(f2(x)),
                                    top = as.numeric(f2(top)), lines = lines, al = al)
  }
  for (r in seq_len(nrow(L1))) {
    ln <- L1$lines[[r]]
    put(L1$id[r], L1$cx[r], Y1 - 20 - LH * length(ln), c(ln, val(L1$n[r])))
  }
  for (r in seq_len(nrow(MID))) if (MID$kind[r] == "tab") {
    ln <- MID$lines[[r]]
    put(MID$id[r], MID$cx[r], YM - 20 - MID$lvl[r] * 84 - LH * length(ln),
        c(ln, val(MID$n[r])))
  }
  for (r in seq_len(nrow(MID))) if (MID$kind[r] == "ghost" && length(MID$lines[[r]])) {
    ly <- if (MID$below[r] == 1) BM + 42 else YM + barH / 2 - 3
    put(MID$id[r], MID$cx[r], ly, c(MID$lines[[r]][1], val(MID$n[r])))
  }
  for (r in seq_len(nrow(L3)))
    put(L3$id[r], L3$cx[r], Y3 + barH + 14, c(L3$lines[[r]], val(L3$n[r])))

  # legend: a 2x2 key (actor x stance)
  LGX <- c(440, 625); CELLW <- 56; CELLH <- 40; LY <- legendY
  ROWY <- c(LY + 48, LY + 98); LABX <- 348; VRULE <- 366
  leg <- "<g>"
  for (e in list(c("or", "lean", 1, 1), c("bl", "lean", 2, 1),
                 c("or", "counter", 1, 2), c("bl", "counter", 2, 2))) {
    base <- HUE[[e[1]]][["tab"]]
    leg <- paste0(leg, '<rect x="', f2(LGX[as.integer(e[3])] - CELLW / 2), '" y="',
                  ni(ROWY[as.integer(e[4])]), '" width="', ni(CELLW), '" height="', ni(CELLH),
                  '" fill="', if (e[2] == "counter") hatch(base) else base, '"></rect>')
  }
  leg <- paste0(leg,
    '<line x1="185" y1="', ni(LY + 36), '" x2="', ni(LGX[2] + CELLW / 2 + 8), '" y2="',
    ni(LY + 36), '" stroke="#c8c8c8" stroke-width="1.5"></line>',
    '<line x1="', ni(VRULE), '" y1="', ni(LY - 46), '" x2="', ni(VRULE), '" y2="',
    ni(ROWY[2] + CELLH + 8), '" stroke="#c8c8c8" stroke-width="1.5"></line>',
    '<line x1="230" y1="', ni(LY - 30), '" x2="330" y2="', ni(LY + 24),
    '" stroke="#c8c8c8" stroke-width="1.5"></line></g>')
  body <- paste0(body, leg)
  put("legCornerActor", LABX, LY, "actor", "right")
  put("legCornerStance", 193, LY + 27, "stance", "left")
  put("legColAi", LGX[1], LY, "AI acts")
  put("legColHu", LGX[2], LY, "Human acts")
  put("legRowLean", LABX, ROWY[1] + 28, "AI's prior", "right")
  put("legRowCounter", LABX, ROWY[2] + 28, "the other side", "right")

  # labels on one baseline that would collide: push the right-hand one right
  estW <- function(L) max(nchar(L$lines)) * FS * 0.5
  free <- which(vapply(LAB, function(L) is.na(L$al), logical(1)))
  tops <- vapply(LAB[free], `[[`, numeric(1), "top")
  for (tp in unique(tops)) {
    row <- free[tops == tp]
    row <- row[order(vapply(LAB[row], `[[`, numeric(1), "cx"), method = "radix")]
    if (length(row) > 1) for (i in 2:length(row)) {
      need <- estW(LAB[[row[i - 1]]]) / 2 + estW(LAB[[row[i]]]) / 2 + 14
      if (LAB[[row[i]]]$cx - LAB[[row[i - 1]]]$cx < need)
        LAB[[row[i]]]$cx <- as.numeric(f2(LAB[[row[i - 1]]]$cx + need))
    }
  }
  esc <- function(s) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", s, fixed = TRUE),
                                            fixed = TRUE), fixed = TRUE)
  txt <- paste0('<g font-family="Helvetica, Arial, sans-serif" font-size="', FS,
                '" font-weight="400" fill="#1a1a1a" text-anchor="middle" stroke="white" ',
                'stroke-width="3.5" paint-order="stroke" stroke-linejoin="round">')
  for (L in LAB) for (i in seq_along(L$lines)) {
    anchor <- if (is.na(L$al)) "" else if (L$al == "left") ' text-anchor="start"'
              else ' text-anchor="end"'
    txt <- paste0(txt, '<text x="', f2(L$cx), '" y="', f2(L$top + 20 + (i - 1) * LH), '"',
                  anchor, ">", esc(L$lines[i]), "</text>")
  }
  txt <- paste0(txt, "</g>")
  defs <- paste0("<defs>", paste0('<pattern id="', names(pats),
    '" width="8.3" height="8.3" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">',
    '<rect width="3.8" height="8.3" fill="', pats, '"></rect></pattern>', collapse = ""),
    "</defs>")
  head <- paste0('<svg xmlns="http://www.w3.org/2000/svg" width="', W, '" height="', H,
                 '" viewBox="0 0 ', W, " ", H, '">', defs,
                 '<rect width="', W, '" height="', H, '" fill="white"></rect>')
  paste0(head, body, txt, "</svg>")
}

svg <- build_sankey(TOT = v$total)
# widen the viewBox so the leftmost label sits inside the drawing
svg <- sub('width="1330" height="1215" viewBox="0 0 1330 1215"',
           'width="1430" height="1235" viewBox="-85 -10 1430 1235"', svg, fixed = TRUE)
stopifnot(grepl('viewBox="-85 -10', svg, fixed = TRUE))
writeLines(svg, file.path(out_figures, "sample_flow.svg"), sep = "")
cat("wrote", file.path(out_figures, "sample_flow.svg"), "\n")
