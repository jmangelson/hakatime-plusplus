import _ from "lodash";
import m from "mithril";

import TimeRange from "./TimeRange";
import utils from "../utils.js";
import * as auth from "../auth";

const Model = {
    classes: [],
    initialized: false,
    clear: () => {
        Model.classes = null
    },
    initConfig: () => {
        Model.classes = [{class_name: 'A'},
                         {class_name: 'B'}]

        console.log("Init Config");
        Model.initilized = true;
        Model.fetchCurrentConfig();
    },
    fetchCurrentConfig: () => {
        // If it was triggered by a click event.
    }
};

export default Model;
