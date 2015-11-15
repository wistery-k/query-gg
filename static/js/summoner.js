var tdu = function(x) {
    return Math.floor(x * 100) / 100;
};

var findTeam = function(match, teamId) {
    var teams = match["teams"];
    for (var i = 0; i < teams.length; i++) {
        if (teams[i]["teamId"] === teamId) {
            return teams[i];
        }
    }
    return null;
};

var findAdc = function(match, teamId) {
    var participants = match["participants"];
    for (var idx = 0; idx < participants.length; idx++) {
        var p = participants[idx];
        if (p["teamId"] == teamId && p["timeline"] && p["timeline"]["role"] === "DUO_CARRY") {
            return p;
        }
    }
    return null;
};

var findMyParticipantId = function(participantIdentities, summonerId) {
    for (var idx = 0; idx < participantIdentities.length; idx++) {
        if (participantIdentities[idx]["player"]["summonerId"] == summonerId) {
            return participantIdentities[idx]["participantId"];
        }
    }
    return null;
};

var findMe = function(match, summonerId) {
    var participantId = findMyParticipantId(match["participantIdentities"], summonerId);
    if (participantId == null) {
        return null;
    }
    var participants = match["participants"];
    for (var idx = 0; idx < participants.length; idx++) {
        var p = participants[idx];
        if (p["participantId"] == participantId) {
            return p;
        }
    }
    return null;
};

var createMatchResult = function(match, realm) {
    var res = $("<div>");

    if (match && match["result"]) {
        res.addClass("match-" + match["result"]);
    } else {
        res.addClass("match-error");
    }

    var header = $("<div>", {"class": "match-result-header"});
    var body = $("<div>", {"class": "match-result-body"});
    res.append(header).append(body);
    if (match) {
        /* Champion icon */
        header.append((match["queue"] || "UNKNOWN") + " " + (match["lane"] || "UNKNOWN") + "-" + (match["role"] || "UNKNOWN"));
        if (realm && realm["cdn"] && match["champion"] && match["champion"]["image"] && match["champion"]["image"]["full"]) {
            body.append($("<img>", {
                "src": realm["cdn"] + "/" + realm["v"] + "/img/champion/" + match["champion"]["image"]["full"],
                "width": 60,
                "height": 60
            }));
        } else {
            body.append($("<img>", {
                "src": "/LoadFailed.png",
                "width": 60,
                "height": 60
            }));
        }

        /* Champion name */
        if (match["champion"] && match["champion"]["name"]) {
            body.append(match["champion"]["name"]);
        } else {
            body.append("unkonwn");
        }

        for (var i = 1; i <= 2; i++) {
            var img = $("<img>", {"width":30, "height":30});
            if (match["spell" + i] && match["spell" + i]["image"] && match["spell" + i]["image"]["full"]) {
                img.attr("src", realm["cdn"] + "/" + realm["v"] + "/img/spell/" + match["spell" + i]["image"]["full"]);
            } else {
                img.attr("src", "/LoadFailed.png");
            }
            body.append(img);
        }

        for (var i = 0; i < 7; i++) {
            var img = $("<img>", {"width":30, "height":30});
            if (match["item" + i]) {
                img.attr("src", match["item" + i]);
            } else {
                img.attr("src", "/img/minus.png");
            }
            body.append(img);
        }

    }
    return res;
};

var filters = {
    "all": function(match) {
        return true;
    },
    "top": function(match) {
        return match["lane"] === "TOP";
    },
    "jungle": function(match) {
        return match["lane"] === "JUNGLE";
    },
    "mid": function(match) {
        return match["lane"] === "MID";
    },
    "adc": function(match) {
        return match["role"] === "DUO_CARRY";
    },
    "support": function(match) {
        return match["role"] === "DUO_SUPPORT";
    },
    "nautilus": function(match) {
        return match["champion"] === 111;
    }
};

window.onload = function(){
    console.log("summoner.js");

    $("#" + filter).addClass("active");

    $.ajaxSetup({"async": false, "dataType": "json"});

    var matchlistContainer = $("#match-list");

    var realm = $.get("/rpc/realm").responseJSON;

    var summoner = $.get("/rpc/summoner/by-name/" + name).responseJSON;
    var id = summoner[name]["id"];
    var matchlist = $.get("/rpc/matchlist/" + id + "?update=" + update).responseJSON["matches"];
    var filteredMatchList = [];
    for (var idx = 0; idx < matchlist.length; idx++) {
        var match = matchlist[idx];
        if (!filters[filter](match)) {
            continue;
        }
        $.extend(match, $.get("/rpc/match/" + match["matchId"]).responseJSON);
        if (!match.hasOwnProperty("participantIdentities")) {
            console.log("match.participantIdentities is null. This should not happen.");
            continue;
        }
        var me = findMe(match, id);
        if (me == null) {
            console.log("me is null. This should not happen.");
            continue;
        }
        match["result"] = me["stats"]["winner"] ? "win" : "lose";
        match["spell1"] = $.get("/rpc/spell/" + me["spell1Id"]).responseJSON;
        match["spell2"] = $.get("/rpc/spell/" + me["spell2Id"]).responseJSON;

        for (var i = 0; i < 7; i++) {
            var item = "item" + i;
            var itemId = me["stats"][item];

            if ([3260, 3265, 3270, 3250, 3279, 3255, 3275].indexOf(itemId) >= 0) {
                match[item] = "/img/minus.png";
            } else if (itemId != 0) {
                var itemInfo = $.get("/rpc/item/" + itemId).responseJSON;
                match[item] = realm["cdn"] + "/" + realm["v"] + "/img/item/" + itemInfo["image"]["full"];
            } else {
                match["item"] = "/img/minus.png";
            }
        }

        match["champion"] = $.get("/rpc/champion/" + match["champion"]).responseJSON;
        //if (!filters[filter](match)) {
        //    continue;
        //}
        matchlistContainer.append(createMatchResult(match, realm));

        console.log(match);
        filteredMatchList.push(match);
    }

    /* calculate metrics from filteredMatchList */
    var csPerMin = 0;
    var csPerMinCount = 0;
    var expPerMin = 0;
    var expPerMinCount = 0;
    var goldPerMin = 0;
    var goldPerMinCount = 0;

    var csPerMinAdc = 0;
    var csPerMinCountAdc = 0;
    var expPerMinAdc = 0;
    var expPerMinCountAdc = 0;
    var goldPerMinAdc = 0;
    var goldPerMinCountAdc = 0;
    var tankDiffPerMinAdc = 0;
    var tankDiffPerMinCountAdc = 0;

    var dragonKill = 0;
    var dragonKillCount = 0;

    var championKill = 0;
    var championKillCount = 0;

    var death = 0;
    var deathCOunt = 0;

    var assist = 0;
    var assistCount = 0;

    var killContribution = 0;
    var killContributionCount = 0;

    for (var i = 0; i < filteredMatchList.length; i++) {
        var match = filteredMatchList[i];
        var me = findMe(match, id);
        if (me == null) {
            console.log("me is null. This should not happen.");
            continue;
        }
        if (me["timeline"] &&
            me["timeline"]["creepsPerMinDeltas"] &&
            me["timeline"]["creepsPerMinDeltas"]["zeroToTen"]
        ) {
            csPerMin += me["timeline"]["creepsPerMinDeltas"]["zeroToTen"];
            csPerMinCount += 1;
        }
        if (me["timeline"] &&
            me["timeline"]["goldPerMinDeltas"] &&
            me["timeline"]["goldPerMinDeltas"]["zeroToTen"]
        ) {
            goldPerMin += me["timeline"]["goldPerMinDeltas"]["zeroToTen"];
            goldPerMinCount += 1;
        }
        if (me["timeline"] &&
            me["timeline"]["xpPerMinDeltas"] &&
            me["timeline"]["xpPerMinDeltas"]["zeroToTen"]
        ) {
            expPerMin += me["timeline"]["xpPerMinDeltas"]["zeroToTen"];
            expPerMinCount += 1;
        }

        if (me["stats"] && me["stats"]["kills"]) {
            championKill += me["stats"]["kills"];
            championKillCount += 1;
        }

        if (me["stats"] && me["stats"]["deaths"]) {
            death += me["stats"]["deaths"];
            deathCOunt += 1;
        }

        if (me["stats"] && me["stats"]["assists"]) {
            assist += me["stats"]["assists"];
            assistCount += 1;
        }

        if (me["stats"] && me["stats"]["kills"] && me["stats"]["assists"]) {
            var ka = me["stats"]["kills"] + me["stats"]["assists"];
            var teamKill = 0;
            var participants = match["participants"];
            for (var j = 0; j < participants.length; j++) {
                var p = participants[j];
                if (p["teamId"] === me["teamId"]) {
                    teamKill += p["stats"]["kills"];
                }
            }
            if (teamKill > 0) {
                killContribution += (ka / teamKill);
                killContributionCount += 1;
            }
        }

        var adc = findAdc(match, me["teamId"]);
        if (adc == null) {
            console.log("Adc is null. This happens occasionally because Riot's estimation algorithm sometimes doesn't work.");
            continue;
        }
        if (adc["timeline"] &&
            adc["timeline"]["creepsPerMinDeltas"] &&
            adc["timeline"]["creepsPerMinDeltas"]["zeroToTen"]
        ) {
            csPerMinAdc += adc["timeline"]["creepsPerMinDeltas"]["zeroToTen"];
            csPerMinCountAdc += 1;
        }
        if (adc["timeline"] &&
            adc["timeline"]["goldPerMinDeltas"] &&
            adc["timeline"]["goldPerMinDeltas"]["zeroToTen"]
        ) {
            goldPerMinAdc += adc["timeline"]["goldPerMinDeltas"]["zeroToTen"];
            goldPerMinCountAdc += 1;
        }
        if (adc["timeline"] &&
            adc["timeline"]["xpPerMinDeltas"] &&
            adc["timeline"]["xpPerMinDeltas"]["zeroToTen"]
        ) {
            expPerMinAdc += adc["timeline"]["xpPerMinDeltas"]["zeroToTen"];
            expPerMinCountAdc += 1;
        }

        if (adc["timeline"] &&
            adc["timeline"]["damageTakenDiffPerMinDeltas"] &&
            adc["timeline"]["damageTakenDiffPerMinDeltas"]["zeroToTen"]
        ) {
            tankDiffPerMinAdc += adc["timeline"]["damageTakenDiffPerMinDeltas"]["zeroToTen"];
            tankDiffPerMinCountAdc += 1;
        }

        var team = findTeam(match, me["teamId"]);
        if (!team) {
            console.log("team is null. This should not happen.");
            continue;
        }

        if (team["dragonKills"]) {
            dragonKill += team["dragonKills"];
            dragonKillCount += 1;
        }
    }
    $("#cs-per-min").text(tdu(csPerMin / csPerMinCount));
    $("#gold-per-min").text(tdu(goldPerMin / goldPerMinCount));
    $("#exp-per-min").text(tdu(expPerMin / expPerMinCount));

    $("#cs-per-min-adc").text(tdu(csPerMinAdc / csPerMinCountAdc));
    $("#gold-per-min-adc").text(tdu(goldPerMinAdc / goldPerMinCountAdc));
    $("#exp-per-min-adc").text(tdu(expPerMinAdc / expPerMinCountAdc));
    $("#tank-diff-per-min-adc").text(tdu(tankDiffPerMinAdc / tankDiffPerMinCountAdc));
    $("#dragon-kill").text(tdu(dragonKill / dragonKillCount));
    $("#champion-kill").text(tdu(championKill / championKillCount));
    $("#death").text(tdu(death / deathCOunt));
    $("#assist").text(tdu(assist / assistCount));
    $("#kill-contribution").text(tdu(killContribution / killContributionCount));
    console.log(dragonKill, dragonKillCount);
};