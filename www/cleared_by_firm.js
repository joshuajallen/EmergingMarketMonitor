
/* Reset fuction will need to be visible elsewhere */
var resetFilters = null;

Shiny.addCustomMessageHandler("cleared_by_firm_data",

  function(message){

    /* Constants and helper functions */
    var singleColour = "#c4d9e0";
    var colourRamp = [
      "#4a7e8f",
      "#cf395c",
      "#0f7cbf",
      "#a9c9d3",
      "#3b855f",
      "#b25395",
      "#2f4f5b",
      "#b65e19"
    ];

    resetFilters = function() {
        dc.filterAll();
        redrawAllContent();
    };

    /* Do not show zero height bars */
    function remove_empty_bins(sourceGroup, filterType) {
        function non_zero_pred(d) {
            if ( filterType > 0 ) {
                return d.value > 0;
            } else if ( filterType < 0 ) {
    			return d.value < 0;
            }
            return Math.abs(d.value - 0) > 1e-8;
        }
        return {
            all: function () {
                return sourceGroup.all().filter(non_zero_pred);
            },
            top: function(n) {
                return sourceGroup.top(Infinity)
                  .filter(non_zero_pred).slice(0, n);
            }
        };
    }

    /* Filter a dimension based on a text input */
    function textFilter(q, dim) {
        dim.filter(null);
        var re = new RegExp(q, "i");
        if (q !== "") {
            dim.filter(function(d) { return d.search(re) > -1; });
        } else {
            dim.filterAll();
        }
        redrawAllContent();
    }

    /* Add an axis title */
    function AddXAxis(chartToUpdate, displayText) {
        chartToUpdate.svg()
            .append("text")
            .attr("class", "x-axis-label")
            .attr("text-anchor", "middle")
            .attr("x", chartToUpdate.width()*0.45)
            .attr("y", chartToUpdate.height())
            .text(displayText);
    }

    /* Refresh all charts after a filter update */
    function redrawAllContent() {
        dc.renderAll();
        AddXAxis(clearingMemberChart, "GBP trn");
        AddXAxis(CCPNameChart,        "GBP trn");
        AddXAxis(maturityChart,       "GBP trn");
    }

    // var dateFormat = d3.time.format("%Y-%m-%d");
    // var humanFormat = d3.time.format("%e %b %y");

    var data = JSON.parse(message);
    var cFilter = crossfilter(data);
    var all = cFilter.groupAll();

    /* Charts */

    var clearingMemberChart = dc.rowChart("#clearing-member-row-chart")
    var assetClassChart = dc.pieChart("#asset-class-chart");
    var CCPEEAChart = dc.pieChart("#ccp-eea-chart");
    var maturityChart = dc.rowChart("#maturity-chart");
    var CCPNameChart = dc.rowChart("#ccp-name-chart");

    /* Checkboxes */

    var venueClassificationBoxes = dc.cboxMenu("#venue-classification-boxes");

    /* Dimensions */

    var clearingMemberDimension = cFilter.dimension(function(d) {
        return d.ClearingMemberName;
    });
    var clearingMemberGroup = clearingMemberDimension.group()
          .reduceSum(dc.pluck("AbsNotionalGBPModified"));
    var filteredClearingMemberGroup =
      remove_empty_bins(clearingMemberGroup, 0);
    var clearingMemberFilterDimension = cFilter.dimension(function(d) {
      return d.ClearingMemberName;
    });
    $("#clearingMemberSearch").on('input', function () {
        textFilter(this.value, clearingMemberFilterDimension);
//        refreshTable(); TODO:
    });

    var assetClassDimension = cFilter.dimension(function(d) {
        return d.AssetClass;
    });
    var assetClassGroup = assetClassDimension.group()
          .reduceSum(dc.pluck("AbsNotionalGBPModified"));

    var CCPEEADimension = cFilter.dimension(function(d) {
        if ( d.CCPEEA === "EU but non euro area") {
          return "Non-EUR EU";
        }
        return d.CCPEEA;
    });
    var CCPEEAGroup = CCPEEADimension.group()
          .reduceSum(dc.pluck("AbsNotionalGBPModified"));

    var maturityDimension = cFilter.dimension(function(d) {
        return d.MaturityBucket;
    });
    var maturityGroup = maturityDimension.group()
          .reduceSum(dc.pluck("AbsNotionalGBPModified"));
    var filteredMaturityGroup =
      remove_empty_bins(maturityGroup, 0);

    var venueClassificationDimension = cFilter.dimension(function(d) {
        if ( d.VenueClassification == "NA") {
          return "Unknown";
        }
        return d.VenueClassification;
    });
    var venueClassificationGroup = venueClassificationDimension.group();

    var CCPNameDimension = cFilter.dimension(function(d) {
        return d.CCPName;
    });
    var CCPNameGroup = CCPNameDimension.group()
          .reduceSum(dc.pluck("AbsNotionalGBPModified"));
    var filteredCCPNameGroup = remove_empty_bins(CCPNameGroup, 0);
    var CCPNameFilterDimension = cFilter.dimension(function(d) {
      return d.CCPName;
    });
    $("#ccpNameSearch").on('input', function () {
        textFilter(this.value, CCPNameFilterDimension);
        // refreshTable(); TODO:
    });

    clearingMemberChart
      .height(595)
      .width(300)
      .dimension(clearingMemberDimension)
      .group(filteredClearingMemberGroup)
      .cap(35)
      .elasticX(true)
      .colors(singleColour)
      .margins( {top: 10, right: 50, bottom: 30, left: 5})
      .ordering(function(d) { return -d.value; })
      .label(function (d) { return d.key === "" ? "N/A" : d.key; })
      .title(function (d) {
        return d.key === "" ? "N/A" : d.key + ": GBP " +
          d.value.toLocaleString('en-GB',
            {maximumFractionDigits: 2}) + " trn";
      })
      .xAxis().ticks(4);


    assetClassChart
      .height(170)
      .width(280)
      .dimension(assetClassDimension)
      .group(assetClassGroup)
      .radius(65)
      .innerRadius(0)
      .colors(d3.scaleOrdinal().range(colourRamp))
      .label(function (d) {
        var name = d.key;
        if ( name === "" ) {
          name = "Missing";
        }
        return name;
      })
      .title(function (d) {
        return d.key === "" ? "Missing" : d.key + ": GBP " +
          d.value.toLocaleString('en-GB',
            {maximumFractionDigits: 2}) + " trn";
      })
      .minAngleForLabel(10)
      .legend(dc.legend().x(0).y(0).gap(5).legendText(function (d) {
        var legend = d.name;
        if ( legend === "" ) {
          legend = "Missing";
        }
        return legend;
      }));

    CCPEEAChart
      .height(170)
      .width(280)
      .dimension(CCPEEADimension)
      .group(CCPEEAGroup)
      .radius(65)
      .innerRadius(0)
      .colors(d3.scaleOrdinal().range(colourRamp))
      .label(function (d) {
        var name = d.key;
        if ( name === "" ) {
          name = "Missing";
        }
        return name;
      })
      .title(function (d) {
        return d.key === "" ? "Missing" : d.key + ": GBP" +
          d.value.toLocaleString('en-GB',
            {maximumFractionDigits: 2}) + " trn";
      })
      .minAngleForLabel(10)
      .legend(dc.legend().x(0).y(0).gap(5).legendText(function (d) {
        var legend = d.name;
        if ( legend === "" ) {
          legend = "Missing";
        } else if ( legend === "Unspecified CCP") {
          legend = "Unknown";
        }
        return legend;
      }));


    maturityChart
        .height(170)
        .width(280)
        .dimension(maturityDimension)
        .group(filteredMaturityGroup)
        .cap(8)
        .elasticX(true)
        .colors(singleColour)
        .margins( {top: 10, right: 50, bottom: 30, left: 5})
        .ordering(function(d) { return -d.value; })
        .label(function (d) {
            if ( d.key === "" ) {
                return "Unknown";
            }
            return d.key;
        })
        .title(function (d) {
            var name = d.key;
            if ( d.key === "" ) {
                name = "Unknown";
            }
            return name + ": GBP " +
              d.value.toLocaleString('en-GB',
                {maximumFractionDigits: 2}) + " trn";
        })
        .xAxis().ticks(4);

    CCPNameChart
      .height(320)
      .width(300)
      .dimension(CCPNameDimension)
      .group(filteredCCPNameGroup)
      .cap(18)
      .elasticX(true)
      .colors(singleColour)
      .margins( {top: 10, right: 50, bottom: 30, left: 5})
      .ordering(function(d) { return -d.value; })
      .label(function (d) { return d.key === "" ? "N/A" : d.key; })
      .title(function (d) {
        return d.key === "" ? "N/A" : d.key + ": GBP " +
          d.value.toLocaleString('en-GB',
            {maximumFractionDigits: 2}) + " trn";
      })
      .xAxis().ticks(4);

    venueClassificationBoxes
      .dimension(venueClassificationDimension)
      .group(venueClassificationGroup)
      .multiple(true)
      .controlsUseVisibility(true);

/*
    var dataTable = $("#data-table").dataTable({
        "bPaginate": true,
        "bLengthChange": true,
        "bFilter": false,
        "bSort": true,
        "order": [[0, "desc"], [1, "desc"]],
        "bInfo": false,
        "bAutoWidth": false,
        "bDeferRender": true,
        "aaData": dateDimension.top(Infinity),
        "bDestroy": true,
        "lengthMenu": [[10, 50, 100, -1], [10, 50, 100, "All"]],
        "pageLength": 50,
        "aoColumns": [
            {"mData": "Date",                   "sClass": "centred-date"},
            {"mData": "Last Update Time",       "sClass": "centred-date"},
            {"mData": "Fill Status",            "sDefaultContent": ""},
            {"mData": "Market Segment",         "sDefaultContent": ""},
            {"mData": "Instrument Class",       "sDefaultContent": ""},
            {"mData": "Description",            "sDefaultContent": ""},
            {"mData": "Currency",               "sDefaultContent": ""},
            {"mData": "Verb",                   "sDefaultContent": ""},
            {"mData": "Price",                  "sDefaultContent": ""},
            {"mData": "Yield",                  "sDefaultContent": ""},
            {"mData": "Euro-equivalent Volume", "sDefaultContent": ""}
        ],
        "columnDefs": [
            {
              "className": "dt-right",
              "targets": [8, 9, 10]
            }
        ]
    });

*/

    /* Do not show an popup when the datatable is empty */

/*
    $.fn.dataTable.ext.errMode = "none";

    $("#data-table").on("error.dt", function ( e, settings, techNote, message ) {
      console.log("DataTable Message: ", message );
    });


    function refreshTable() {
        dc.events.trigger(function () {
            allData = dateDimension.top(Infinity);
            dataTable.fnClearTable();
            dataTable.fnAddData(allData);
            dataTable.fnDraw();
        });
    }

    for (var i = 0; i < dc.chartRegistry.list().length; i++) {
        var chartI = dc.chartRegistry.list()[i];
        chartI.on("filtered", refreshTable);
    }
*/
    // Set the output div visible now we are ready

    var d = document.getElementById("interdealer-trade-div");
    d.style.display = "block";

    redrawAllContent();

  }

);