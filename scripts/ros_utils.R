## ---- ros_utils.R: shared by 48_ros_inputs.R (Tuesday, Stratus) and 49_ros_page.R (Stratus + daily GitHub refresh) ----
## Base R + dplyr only (runs on the GitHub runner). Same methods as the back-test 47_ros_backtest.R (2026-10-03):
##   * market power ratings from posted lines (decay 0.70 / week, x0.5 per offseason, ridge 0.2 chosen on 2017-20)
##   * climate normals per stadium x date x kickoff hour from the Open-Meteo archive (cached in data/ros/climate)

ROS_TEAMS_MAP <- c(OAK = "LV", SD = "LAC", STL = "LA")
ros_norm_team <- function(x) { i <- x %in% names(ROS_TEAMS_MAP); x[i] <- ROS_TEAMS_MAP[x[i]]; x }
ROS_DEFAULT_P <- list(decay = 0.70, off = 0.5, lambda = 0.2)
# roof of an upcoming game: 48 uses the stadium's most common recorded roof, as 40 and 50 do (all five retractables: closed)
ROS_STADIUMS <- data.frame(
  stadium_id = c("GNB00", "BUF00", "CLE00", "JAX00", "MIA00", "NYC01", "PIT00", "WAS00", "SFO01", "TAM00", "DEN00", "CHI98", "KAN00",
                 "BAL00", "CAR00", "CIN00", "NAS00", "BOS00", "PHI00", "SEA00", "LON00", "LON01", "LON02", "MEX00", "SAO00", "GER00",
                 "FRA00", "RIO00", "MAD01", "DUB00", "BER00"),
  lat = c(44.5013, 42.7738, 41.5061, 30.3239, 25.9580, 40.8135, 40.4468, 38.9078, 37.4030, 27.9759, 39.7439, 41.8623, 39.0489,
          39.2780, 35.2258, 39.0955, 36.1665, 42.0909, 39.9008, 47.5952, 51.5560, 51.4560, 51.6043, 19.3029, -23.5453, 48.2188,
          50.0686, -22.9121, 40.4531, 53.3607, 52.5147),
  lon = c(-88.0622, -78.7870, -81.6995, -81.6373, -80.2389, -74.0745, -80.0158, -76.8645, -121.9700, -82.5033, -105.0201, -87.6167, -94.4839,
          -76.6227, -80.8528, -84.5161, -86.7713, -71.2643, -75.1675, -122.3316, -0.2796, -0.3415, -0.0664, -99.1505, -46.4742, 11.6247,
          8.6455, -43.2302, -3.6883, -6.2512, 13.2395), stringsAsFactors = FALSE)

# one row per team-game from an nflverse schedule: implied totals from the closing (or current) line
ros_team_games <- function(g) {
  g$neutral <- g$location %in% "Neutral"
  side <- function(home) data.frame(game_id = g$game_id, season = g$season, week = g$week, game_type = g$game_type,
    gameday = as.Date(g$gameday), gametime = g$gametime, roof = ifelse(is.na(g$roof), "", g$roof), stadium_id = g$stadium_id,
    team = if (home) g$home_team else g$away_team, opp = if (home) g$away_team else g$home_team,
    loc = ifelse(g$neutral, 0, if (home) 1 else -1), spread = if (home) g$spread_line else -g$spread_line, total = g$total_line,
    played = !is.na(g$home_score), stringsAsFactors = FALSE)
  d <- rbind(side(TRUE), side(FALSE))
  d$team <- ros_norm_team(d$team); d$opp <- ros_norm_team(d$opp)
  d$implied <- (d$total + d$spread) / 2
  d[order(d$season, d$week, d$game_id, d$team), ]
}

## ---- market power ratings: implied = league + offense(team) + defense(opp) + home field, weighted ridge ----
# obs: team, opp, loc, implied, season, pos (week index; the run's week = pos_now). Weights decay^(pos_now - pos) x off^(s_now - season).
ros_fit_ratings <- function(obs, s_now, pos_now, p = ROS_DEFAULT_P) {
  teams <- sort(unique(c(obs$team[obs$season >= s_now - 1], obs$opp[obs$season >= s_now - 1])))
  o <- obs[obs$pos <= pos_now & obs$season >= s_now - 3 & obs$team %in% teams & obs$opp %in% teams & !is.na(obs$implied), ]
  w <- p$decay^(pos_now - o$pos) * p$off^(s_now - o$season)
  n <- nrow(o); k <- length(teams); X <- matrix(0, n, 2 + 2 * k); X[, 1] <- 1; X[, 2] <- o$loc
  X[cbind(seq_len(n), 2 + match(o$team, teams))] <- 1; X[cbind(seq_len(n), 2 + k + match(o$opp, teams))] <- 1
  sw <- sqrt(w); A <- crossprod(X * sw); diag(A)[-(1:2)] <- diag(A)[-(1:2)] + p$lambda
  b <- drop(solve(A, crossprod(X * sw, o$implied * sw)))
  list(mu = b[1], h = b[2], o = setNames(b[2 + seq_len(k)], teams), dv = setNames(b[2 + k + seq_len(k)], teams), n = n, p = p)
}
ros_implied <- function(R, team, opp, loc) unname(R$mu + R$h * loc + R$o[team] + R$dv[opp])

## ---- Open-Meteo historical archive (hourly, ET) per stadium x season (Sep 1 - Jan 31), cached ----
ros_archive_file <- function(dir, st, s) file.path(dir, sprintf("%s_%d.rds", st, s))
ros_fetch_archive <- function(dir, st, s, sleep = 4) {
  f <- ros_archive_file(dir, st, s); if (file.exists(f)) return(readRDS(f))
  xy <- ROS_STADIUMS[ROS_STADIUMS$stadium_id == st, ]; if (!nrow(xy)) return(NULL)
  url <- sprintf(paste0("https://archive-api.open-meteo.com/v1/archive?latitude=%.4f&longitude=%.4f&start_date=%d-09-01&end_date=%d-01-31",
                        "&hourly=temperature_2m,wind_speed_10m,precipitation,snowfall&temperature_unit=fahrenheit&wind_speed_unit=mph",
                        "&precipitation_unit=inch&timezone=America%%2FNew_York"), xy$lat, xy$lon, s, s + 1)
  js <- NULL
  for (k in 1:4) {
    js <- tryCatch(jsonlite::fromJSON(url), error = function(e) e)
    if (!inherits(js, "error")) break
    Sys.sleep(if (grepl("429", conditionMessage(js))) 70 else 5 * k)
  }
  Sys.sleep(sleep)
  if (inherits(js, "error")) { message(sprintf("  archive %s %d failed: %s", st, s, conditionMessage(js))); return(NULL) }
  h <- js$hourly
  out <- data.frame(stadium_id = st, time = h$time, temp = as.numeric(h$temperature_2m), wind = as.numeric(h$wind_speed_10m),
                    precip = as.numeric(h$precipitation), snow = as.numeric(h$snowfall), stringsAsFactors = FALSE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE); saveRDS(out, f); out
}
ros_season_of <- function(d) as.integer(format(d, "%Y")) - (as.integer(format(d, "%m")) < 7)
ros_day_off <- function(d) as.numeric(d - as.Date(paste0(ros_season_of(d), "-09-01")))
# kickoff-window aggregates (temp / wind: kickoff hour + 2 h, as the site's forecasts; rain / snow: 1 h before to 3 h after)
ros_agg_archive <- function(a, kick_hrs) {
  a$date <- as.Date(substr(a$time, 1, 10)); a$hr <- as.integer(substr(a$time, 12, 13))
  do.call(rbind, lapply(kick_hrs, function(kh) {
    x <- a[a$hr >= kh - 1 & a$hr <= kh + 3, ]; core <- x$hr >= kh & x$hr <= kh + 2
    key <- paste(x$stadium_id, x$date)
    data.frame(stadium_id = tapply(x$stadium_id, key, `[`, 1), date = as.Date(tapply(as.character(x$date), key, `[`, 1)), kick_hr = kh,
               temp_ar = tapply(ifelse(core, x$temp, NA), key, mean, na.rm = TRUE), wind_ar = tapply(ifelse(core, x$wind, NA), key, mean, na.rm = TRUE),
               precip = tapply(x$precip, key, sum, na.rm = TRUE), snow = tapply(x$snow, key, sum, na.rm = TRUE), stringsAsFactors = FALSE)
  }))
}

## ---- page helpers ----
ros_esc <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
ros_pct <- function(x) ifelse(is.na(x), "", paste0(round(100 * x), "%"))
`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- the page's script: one matrix per section (D/ST, kickers) x format; views points / rank / vs the week's average ----
ROS_JS <- function() r"---[
(function(){
const S=D.sections,W=D.weeks,PO=D.playoff,$=id=>document.getElementById(id);
let sec=Object.keys(S)[0],fmt=null,view='pts',sortKey='n4',sortDir=-1,pinned=null;
const esc=s=>String(s==null?'':s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
const SRC={live:'live lines',lookahead:'posted lines',projected:'projected',mixed:'some posted'};
function btns(el,label,items,cur,cb){el.innerHTML=label?'<span>'+label+'</span>':'';items.forEach(([k,l])=>{const b=document.createElement('button');b.textContent=l;if(k===cur)b.className='on';b.onclick=()=>cb(k);el.appendChild(b);});}
function fx(x,d){return x==null?'':x.toFixed(d);}
function drawTabs(){btns($('sec'),'',Object.keys(S).map(k=>[k,S[k].title]).concat(S.dst?[['pairs','D/ST pairs']]:[]),sec,
  k=>{sec=k;pinned=null;pPinned=null;tip.style.display='none';try{history.replaceState(null,'',k==='pairs'?'#pairs':location.pathname);}catch(e){}render();});}
function render(){
  drawTabs();const isP=sec==='pairs';$('grid').parentNode.style.display=isP?'none':'';
  document.querySelectorAll('.leg').forEach(x=>x.style.display=isP?'none':'');$('pairs').style.display=isP?'':'none';
  if(isP){renderPairs();return;}
  const sc=S[sec],sys=Object.keys(sc.systems);if(!fmt||!sys.includes(fmt))fmt=sys[0];
  btns($('fmt'),'Format',sys.map(k=>[k,sc.systems[k]]),fmt,k=>{fmt=k;render();});
  btns($('view'),'Show',[['pts','Points'],['rank','Rank'],['dev','vs avg']],view,k=>{view=k;render();});
  const V=sc.vals[fmt],T=sc.teams,val=(t,i)=>{const c=V[t]&&V[t][i];return c?c.v:null;};
  const st=W.map((w,i)=>{const xs=T.map(t=>val(t,i)).filter(x=>x!=null);const m=xs.reduce((a,b)=>a+b,0)/Math.max(1,xs.length);
    const sd=Math.sqrt(xs.reduce((a,b)=>a+(b-m)*(b-m),0)/Math.max(1,xs.length-1))||1;return {m:m,sd:sd,s:xs.slice().sort((a,b)=>b-a),n:xs.length};});
  const rk=(x,i)=>st[i].s.indexOf(x)+1;
  const n3=W.slice(0,3),n4=W.slice(0,4),po=PO.filter(w=>W.includes(w));
  const sumW=(t,ws)=>ws.reduce((a,w)=>{const v=val(t,W.indexOf(w));return a+(v==null?0:v);},0);
  const rows=T.map(t=>{const vs=W.map((w,i)=>val(t,i)).filter(x=>x!=null);
    return {t:t,n3:sumW(t,n3),n4:sumW(t,n4),avg:vs.length?vs.reduce((a,b)=>a+b,0)/vs.length:null,po:po.length?sumW(t,po):null,
            byes:W.filter((w,i)=>!(sc.cells[t]&&sc.cells[t][i]))};});
  const cols=[['n3','Next 3','wks '+n3[0]+'–'+n3[n3.length-1]],['n4','Next 4','wks '+n4[0]+'–'+n4[n4.length-1]],['avg','ROS avg','per game']];
  if(po.length)cols.push(['po','Wk '+po[0]+'–'+po[po.length-1],'playoffs']);
  const cz={};cols.forEach(([k])=>{const xs=rows.map(r=>r[k]).filter(x=>x!=null),m=xs.reduce((a,b)=>a+b,0)/xs.length,
    sd=Math.sqrt(xs.reduce((a,b)=>a+(b-m)*(b-m),0)/Math.max(1,xs.length-1))||1,s=xs.slice().sort((a,b)=>b-a);cz[k]={m:m,sd:sd,s:s};});
  const key=r=>sortKey==='team'?r.t:(sortKey[0]==='w'?val(r.t,+sortKey.slice(1)):r[sortKey]);
  rows.sort((a,b)=>{const x=key(a),y=key(b);if(sortKey==='team')return sortDir*(x<y?-1:x>y?1:0);
    if(x==null&&y==null)return 0;if(x==null)return 1;if(y==null)return -1;return sortDir*(x-y);});
  const bg=z=>'background:rgba(var(--'+(z>=0?'pos':'neg')+'),'+(Math.min(Math.abs(z)/2,1)*0.5).toFixed(2)+')';
  const sk=k=>sortKey===k?' sorted':'';
  let h='<thead><tr><th class="tm'+sk('team')+'" data-k="team">'+(sec==='k'?'Team (kicker)':'D/ST')+'</th>';
  cols.forEach(([k,l,s])=>h+='<th class="sumh'+sk(k)+'" data-k="'+k+'">'+l+'<small>'+s+'</small></th>');
  h+='<th class="gap"></th>';
  W.forEach((w,i)=>h+='<th class="'+sk('w'+i)+'" data-k="w'+i+'">Wk '+w+'<small>'+SRC[D.wsrc[i]]+'</small></th>');
  h+='</tr></thead><tbody>';
  rows.forEach(r=>{const who=sec==='k'&&sc.who[r.t]?'<small>'+esc(sc.who[r.t])+'</small>':'';
    h+='<tr><td class="tm">'+r.t+who+'</td>';
    cols.forEach(([k])=>{const x=r[k];if(x==null){h+='<td class="sum"></td>';return;}const z=(x-cz[k].m)/cz[k].sd,rr=cz[k].s.indexOf(x)+1;
      const tip=(k==='avg'?'Average per remaining game: ':'Total points: ')+x.toFixed(1)+' (rank '+rr+' of '+cz[k].s.length+')'+
        (k!=='avg'&&r.byes.some(w=>(k==='n3'?n3:k==='n4'?n4:po).includes(w))?' · includes a bye (0 pts)':'');
      h+='<td class="sum" style="'+bg(z)+'" title="'+esc(tip)+'">'+x.toFixed(1)+'</td>';});
    h+='<td class="gap"></td>';
    W.forEach((w,i)=>{const g=sc.cells[r.t]&&sc.cells[r.t][i];if(!g){h+='<td class="c bye">BYE</td>';return;}
      const v=val(r.t,i);if(v==null){h+='<td class="c">?<span class="o">'+esc(g.o)+'</span></td>';return;}
      const z=(v-st[i].m)/st[i].sd,txt=view==='pts'?v.toFixed(1):view==='rank'?String(rk(v,i)):((v-st[i].m)>=0?'+':'')+(v-st[i].m).toFixed(1);
      h+='<td class="c'+(g.s==='projected'?' pj':'')+'" style="'+bg(z)+'" data-t="'+r.t+'" data-i="'+i+'">'+txt+'<span class="o">'+esc(g.o)+'</span></td>';});
    h+='</tr>';});
  $('grid').innerHTML=h+'</tbody>';
}
function tipHtml(t,i){const sc=S[sec],g=sc.cells[t][i],c=sc.vals[fmt][t][i];if(!g||!c)return '';
  const T=sc.teams,xs=T.map(u=>{const q=sc.vals[fmt][u]&&sc.vals[fmt][u][i];return q?q.v:null;}).filter(x=>x!=null).sort((a,b)=>b-a);
  const m=xs.reduce((a,b)=>a+b,0)/xs.length;
  let s='<b>'+esc(t)+' · '+esc(sc.systems[fmt])+': '+c.v.toFixed(1)+' pts</b> (rank '+(xs.indexOf(c.v)+1)+' of '+xs.length+', week average '+m.toFixed(1)+')\n'+esc(g.t);
  if(c.d)s+='\n<b>What moves it</b> (vs an average team that week):\n'+esc(c.d);
  return s;}
const tip=$('tip');
function place(x,y){const r=tip.getBoundingClientRect(),W2=window.innerWidth,H2=window.innerHeight;
  tip.style.left=Math.max(8,Math.min(x+14,W2-r.width-8))+'px';tip.style.top=Math.max(8,(y+14+r.height>H2?y-r.height-14:y+14))+'px';}
const grid=$('grid');
grid.addEventListener('mousemove',e=>{const td=e.target.closest('td.c[data-t]');if(!td){if(!pinned)tip.style.display='none';return;}
  if(pinned)return;tip.innerHTML=tipHtml(td.dataset.t,+td.dataset.i);tip.style.display='block';place(e.clientX,e.clientY);});
grid.addEventListener('mouseleave',()=>{if(!pinned)tip.style.display='none';});
grid.addEventListener('click',e=>{const th=e.target.closest('th[data-k]');
  if(th){const k=th.dataset.k;if(sortKey===k)sortDir=-sortDir;else{sortKey=k;sortDir=(k==='team'?1:-1);}render();return;}
  const td=e.target.closest('td.c[data-t]');if(!td){pinned=null;tip.style.display='none';return;}
  if(pinned===td){pinned=null;tip.style.display='none';return;}
  pinned=td;tip.innerHTML=tipHtml(td.dataset.t,+td.dataset.i);tip.style.display='block';const r=td.getBoundingClientRect();place(r.left,r.bottom);});
document.addEventListener('click',e=>{if(pinned&&!e.target.closest('#grid')){pinned=null;tip.style.display='none';}});
const pBg=z=>'background:rgba(var(--'+(z>=0?'pos':'neg')+'),'+(Math.min(Math.abs(z)/2,1)*0.5).toFixed(2)+')';
// ---- D/ST pairs tab (Andrew 2026-10-07): which two D/STs cover each other best over the rest of the season ----
// Rule: each week you start whichever of the two projects higher (a bye = the other one plays; both on bye = 0).
// Pair pts/wk = that total / weeks in the window. Pairing gain = pair total minus the better of the two on its own.
const PE=Math.max.apply(null,PO.length?PO:W);
let pWin='ros',pView='find',pAnchor='',pMetric='pair',hidden=new Set(),pPinned=null;
try{const s=JSON.parse(localStorage.getItem('rosPairs')||'{}');if(s.a)pAnchor=s.a;if(Array.isArray(s.h))hidden=new Set(s.h);}catch(e){}
function pSave(){try{localStorage.setItem('rosPairs',JSON.stringify({a:pAnchor,h:[...hidden]}));}catch(e){}}
function pIdx(){return W.map((w,i)=>[w,i]).filter(([w])=>w<=PE&&(pWin==='ros'||PO.includes(w))).map(([w,i])=>i);}
function pVal(t,i){const c=S.dst.vals[fmt][t]&&S.dst.vals[fmt][t][i];return c?c.v:null;}
function pOpp(t,i){const g=S.dst.cells[t]&&S.dst.cells[t][i];return g?g.o:'bye';}
function pPair(a,b,I){let tot=0,na=0,nb=0;const both=[],pick=[];
  I.forEach(i=>{const x=pVal(a,i),y=b?pVal(b,i):null;
    if(x==null&&y==null){both.push(W[i]);pick.push(null);return;}
    if(y==null||(x!=null&&x>=y)){tot+=x;na++;pick.push(a);}else{tot+=y;nb++;pick.push(b);}});
  return {tot:tot,na:na,nb:nb,both:both,pick:pick};}
function pWk(n){return n===1?'1 wk':n+' wks';}
function pStrip(a,b,p,I,abbr){return I.map((i,k)=>{const who=p.pick[k];
  if(who==null)return '<td class="s bb" data-a="'+a+'" data-b="'+(b||'')+'" data-i="'+i+'">—<small>'+(b?'both bye':'bye')+'</small></td>';
  const v=pVal(who,i),cls=who===a?'pa':'pb';
  return '<td class="s '+cls+(S.dst.cells[who][i].s==='projected'?' pj':'')+'" data-a="'+a+'" data-b="'+(b||'')+'" data-i="'+i+'">'+v.toFixed(1)+
    '<small>'+(abbr?who:esc(pOpp(who,i)))+'</small></td>';}).join('');}
function renderPairs(){
  const sc=S.dst,sys=Object.keys(sc.systems);if(!fmt||!sys.includes(fmt))fmt=sys[0];
  btns($('fmt'),'Format',sys.map(k=>[k,sc.systems[k]]),fmt,k=>{fmt=k;render();});
  btns($('view'),'Show',[['find','Find a partner'],['heat','Heatmap']],pView,k=>{pView=k;pPinned=null;tip.style.display='none';render();});
  const I=pIdx(),nW=Math.max(1,I.length),wl=I.map(i=>W[i]),wr='wks '+wl[0]+'–'+wl[wl.length-1];
  const solo={};sc.teams.forEach(t=>solo[t]=pPair(t,null,I).tot);
  const avail=sc.teams.filter(t=>!hidden.has(t));
  let h='<div class="bar"><div class="grp" id="pwin"></div>';
  if(pView==='find')h+='<label class="grp">Your D/ST <select id="panc"><option value="">— best pairs overall —</option>'+
    sc.teams.map(t=>'<option'+(t===pAnchor?' selected':'')+'>'+t+'</option>').join('')+'</select></label>';
  else h+='<div class="grp" id="pmet"></div>';
  h+='</div><div class="chips"><span>Not available (rostered elsewhere) — click to hide:</span>'+
    sc.teams.map(t=>'<button data-h="'+t+'"'+(hidden.has(t)?' class="off"':'')+'>'+t+'</button>').join('')+
    (hidden.size?'<button data-h="*">show all</button>':'')+'</div>';
  const wkHead=I.map(i=>'<th>Wk '+W[i]+'<small>'+SRC[D.wsrc[i]]+'</small></th>').join('');
  if(pView==='find'&&pAnchor){
    const A=pAnchor,rows=avail.filter(t=>t!==A).map(t=>{const p=pPair(A,t,I);return {t:t,p:p,gain:p.tot-solo[A]};}).sort((x,y)=>y.p.tot-x.p.tot);
    h+='<p class="note">Partners for <b>'+A+'</b> over '+wr+': each week the higher projection starts. <span class="sw pa"></span> '+A+' starts · '+
      '<span class="sw pb"></span> partner starts · <span class="sw bb"></span> both on bye. '+A+' alone: '+(solo[A]/nW).toFixed(2)+' pts/wk.</p>';
    h+='<div class="wrap"><table class="pt"><thead><tr><th>#</th><th class="tm">Partner</th><th>Pair pts/wk<small>'+wr+'</small></th><th>Gain vs '+A+
      '<small>pts/wk</small></th><th>Total<small>'+pWk(I.length)+'</small></th><th>Partner<br>starts</th><th class="gap"></th>'+wkHead+'</tr></thead><tbody>';
    const sp=pPair(A,null,I);
    h+='<tr class="solo"><td></td><td class="tm">'+A+' alone</td><td>'+(solo[A]/nW).toFixed(2)+'</td><td>—</td><td>'+solo[A].toFixed(1)+'</td><td>—</td><td class="gap"></td>'+pStrip(A,null,sp,I,false)+'</tr>';
    rows.forEach((r,k)=>{h+='<tr><td>'+(k+1)+'</td><td class="tm">'+r.t+'</td><td class="sum">'+(r.p.tot/nW).toFixed(2)+'</td><td class="sum" style="'+
      pBg(r.gain/nW/0.6)+'">+'+(r.gain/nW).toFixed(2)+'</td><td>'+r.p.tot.toFixed(1)+'</td><td>'+pWk(r.p.nb)+(r.p.both.length?' <span title="both on bye: wk '+r.p.both.join(', ')+'">⚠</span>':'')+
      '</td><td class="gap"></td>'+pStrip(A,r.t,r.p,I,false)+'</tr>';});
    h+='</tbody></table></div>';
  }else if(pView==='find'){
    const pr=[];for(let x=0;x<avail.length;x++)for(let y=x+1;y<avail.length;y++){const a0=avail[x],b0=avail[y],a=solo[a0]>=solo[b0]?a0:b0,b=a===a0?b0:a0,p=pPair(a,b,I);
      pr.push({a:a,b:b,p:p,fit:p.tot-solo[a]});}
    pr.sort((u,v)=>v.p.tot-u.p.tot);
    h+='<p class="note">Best pairs among the available D/STs over '+wr+'. Pick your D/ST above to see its best partners. <span class="sw pa"></span> first team starts · <span class="sw pb"></span> second team starts.</p>';
    h+='<div class="wrap"><table class="pt"><thead><tr><th>#</th><th class="tm">Pair</th><th>Pair pts/wk<small>'+wr+'</small></th><th>Pairing gain<small>pts/wk vs better alone</small></th><th>Total<small>'+pWk(I.length)+
      '</small></th><th class="gap"></th>'+wkHead+'</tr></thead><tbody>';
    pr.slice(0,40).forEach((r,k)=>{h+='<tr><td>'+(k+1)+'</td><td class="tm">'+r.a+' + '+r.b+'</td><td class="sum">'+(r.p.tot/nW).toFixed(2)+'</td><td class="sum" style="'+
      pBg(r.fit/nW/0.6)+'">+'+(r.fit/nW).toFixed(2)+'</td><td>'+r.p.tot.toFixed(1)+'</td><td class="gap"></td>'+pStrip(r.a,r.b,r.p,I,true)+'</tr>';});
    h+='</tbody></table></div>';
  }else{
    const T=avail.slice().sort((x,y)=>solo[y]-solo[x]),M={},vals=[];
    T.forEach(a=>{M[a]={};T.forEach(b=>{if(a===b)return;const p=pPair(a,b,I),v=pMetric==='pair'?p.tot/nW:(p.tot-Math.max(solo[a],solo[b]))/nW;M[a][b]={v:v,p:p};vals.push(v);});});
    const m=vals.reduce((s,x)=>s+x,0)/Math.max(1,vals.length),sd=Math.sqrt(vals.reduce((s,x)=>s+(x-m)*(x-m),0)/Math.max(1,vals.length-1))||1,mx=Math.max.apply(null,vals.concat([0.01]));
    h+='<p class="note">'+(pMetric==='pair'?'<b>Pair pts/wk</b>: average weekly points from starting the better of the two each week ('+wr+'). Green = better than the average pair. Best for choosing a pair.':
      '<b>Pairing gain</b>: how much the pair adds over the better of the two on its own (pts/wk, '+wr+'). High = schedules that cover each other (byes and soft weeks fall in different weeks), regardless of how good the teams are.')+
      ' Teams are sorted by their own projection over the window (best at top-left). Hover for details; click a cell to open that team&#39;s partner list.</p>';
    h+='<div class="wrap"><table class="hm"><thead><tr><th class="tm">D/ST</th><th title="the team on its own">Alone</th>'+T.map(t=>'<th>'+t+'</th>').join('')+'</tr></thead><tbody>';
    T.forEach(a=>{h+='<tr><td class="tm">'+a+'</td><td class="dg">'+(solo[a]/nW).toFixed(1)+'</td>';T.forEach(b=>{if(a===b){h+='<td class="dg">—</td>';return;}
      const c=M[a][b],st=pMetric==='pair'?pBg((c.v-m)/sd):'background:rgba(var(--pos),'+(Math.max(0,c.v)/mx*0.6).toFixed(2)+')';
      h+='<td class="hc" style="'+st+'" data-a="'+a+'" data-b="'+b+'">'+c.v.toFixed(1)+'</td>';});h+='</tr>';});
    h+='</tbody></table></div>';
  }
  $('pairs').innerHTML=h+$('pairs_help').innerHTML;
  btns($('pwin'),'Window',[['ros','Rest of season (wk '+Math.min.apply(null,W)+'–'+PE+')'],['po','Playoffs (wk '+PO[0]+'–'+PO[PO.length-1]+')']],pWin,k=>{pWin=k;render();});
  if($('pmet'))btns($('pmet'),'Value',[['pair','Pair pts/wk'],['gain','Pairing gain']],pMetric,k=>{pMetric=k;render();});
  if($('panc'))$('panc').onchange=e=>{pAnchor=e.target.value;pSave();render();};
}
function pTipHtml(el){const a=el.dataset.a,b=el.dataset.b||null,I=pIdx(),nW=Math.max(1,I.length),sys=S.dst.systems[fmt];
  if(el.dataset.i!=null){const i=+el.dataset.i,x=pVal(a,i),y=b?pVal(b,i):null,ln=(t,v)=>'<b>'+t+'</b> '+(v==null?'on bye':v.toFixed(1)+' pts · '+esc(pOpp(t,i)));
    const st=(x==null&&y==null)?'Neither plays':'Start '+((y==null||(x!=null&&x>=y))?a:b);
    return '<b>Week '+W[i]+' · '+esc(sys)+': '+st+'</b>\n'+ln(a,x)+(b?'\n'+ln(b,y):'')+(S.dst.cells[a][i]&&S.dst.cells[a][i].s==='projected'?'\n(projected lines)':'');}
  const p=pPair(a,b,I),sa=pPair(a,null,I).tot,sb=pPair(b,null,I).tot;
  return '<b>'+a+' + '+b+' · '+esc(sys)+': '+(p.tot/nW).toFixed(2)+' pts/wk</b> (weeks '+W[I[0]]+'–'+W[I[I.length-1]]+')\n'+
    'Start '+a+' '+pWk(p.na)+', '+b+' '+pWk(p.nb)+(p.both.length?' · both on bye wk '+p.both.join(', '):'')+'\n'+
    a+' alone '+(sa/nW).toFixed(2)+' · '+b+' alone '+(sb/nW).toFixed(2)+' pts/wk\n'+
    'Gain over '+a+' alone +'+((p.tot-sa)/nW).toFixed(2)+' · over '+b+' alone +'+((p.tot-sb)/nW).toFixed(2)+' pts/wk';}
const pairs=$('pairs');
pairs.addEventListener('mousemove',e=>{const el=e.target.closest('td[data-a]');if(!el){if(!pPinned)tip.style.display='none';return;}
  if(pPinned)return;tip.innerHTML=pTipHtml(el);tip.style.display='block';place(e.clientX,e.clientY);});
pairs.addEventListener('mouseleave',()=>{if(!pPinned)tip.style.display='none';});
pairs.addEventListener('click',e=>{const hb=e.target.closest('button[data-h]');
  if(hb){const t=hb.dataset.h;if(t==='*')hidden.clear();else if(hidden.has(t))hidden.delete(t);else hidden.add(t);pSave();render();return;}
  const hc=e.target.closest('td.hc');if(hc){pAnchor=hc.dataset.a;pView='find';pSave();tip.style.display='none';pPinned=null;render();window.scrollTo(0,pairs.offsetTop-10);return;}
  const el=e.target.closest('td[data-a]');if(!el){pPinned=null;tip.style.display='none';return;}
  if(pPinned===el){pPinned=null;tip.style.display='none';return;}
  pPinned=el;tip.innerHTML=pTipHtml(el);tip.style.display='block';const r=el.getBoundingClientRect();place(r.left,r.bottom);});
document.addEventListener('click',e=>{if(pPinned&&!e.target.closest('#pairs')){pPinned=null;tip.style.display='none';}});
if(location.hash==='#pairs'&&S.dst)sec='pairs';
render();
})();
]---"
