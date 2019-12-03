$("#btn_0").click(function () {
        doCharts(m_data);
    });
$("#btn_1").click(function () {
        doCharts(m_data);
    });
$("#btn_3").click(function () {
        doCharts(m_data);
    });
$("#btn_2").click(function () {
        doCharts(m_data);
    }); function doCharts(alldata) {
    var content = $("#content");
    content.children().remove();
    for (var i = 0; i < alldata.length; i++) {
        setTimeout(function() {}, 10)
        var url = chartUrl(alldata[i], i);
        content.append('' + '<img src="' + url + '" />');
    }
}

function chartUrl(data, i) {
    var res = "http://chart.apis.google.com/chart?chs=600x150&chls=1&cht=lc&chtt=" + i + "&chd=";
    var maxval = Math.max.apply(Math, data);
    return res + simpleEncode(data, maxval);
}

//// Under this line is Google's chart encoding functions, copied and pasted from their site at:
////   http://code.google.com/apis/chart/docs/data_formats.html#encoding_data
var simpleEncoding =
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

// This function scales the submitted values so that
// maxVal becomes the highest value.
function simpleEncode(valueArray, maxValue) {
    var chartData = ['s:'];
    for (var i = 0; i < valueArray.length; i++) {
        var currentValue = valueArray[i];
        if (!isNaN(currentValue) && currentValue >= 0) {
            chartData.push(simpleEncoding.charAt(Math.round((simpleEncoding.length - 1) *
      currentValue / maxValue)));
        }
        else {
            chartData.push('_');
        }
    }
    return chartData.join('');
}

// Same as simple encoding, but for extended encoding.
var EXTENDED_MAP =
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-.';
var EXTENDED_MAP_LENGTH = EXTENDED_MAP.length;
function extendedEncode(arrVals, maxVal) {
    var chartData = 'e:';

    for (i = 0, len = arrVals.length; i < len; i++) {
        // In case the array vals were translated to strings.
        var numericVal = new Number(arrVals[i]);
        // Scale the value to maxVal.
        var scaledVal = Math.floor(EXTENDED_MAP_LENGTH *
        EXTENDED_MAP_LENGTH * numericVal / maxVal);

        if (scaledVal > (EXTENDED_MAP_LENGTH * EXTENDED_MAP_LENGTH) - 1) {
            chartData += "..";
        } else if (scaledVal < 0) {
            chartData += '__';
        } else {
            // Calculate first and second digits and add them to the output.
            var quotient = Math.floor(scaledVal / EXTENDED_MAP_LENGTH);
            var remainder = scaledVal - EXTENDED_MAP_LENGTH * quotient;
            chartData += EXTENDED_MAP.charAt(quotient) + EXTENDED_MAP.charAt(remainder);
        }
    }

    return chartData;
}

