import _ from "lodash";
import m from "mithril";

import TimeRange from "./TimeRange";
import utils from "../utils.js";
import * as auth from "../auth";

const Model = {
    classes: [],
    task_states: [],
    goal_classes: [],
    major_categories: null,
    sub_categories: null,
    subsub_categories: null,
    project_lists: null,
    task_lists: null,
    contexts: null,    
    obj: null,
    initialized: false,
    fetched: false,
    selected_maj_cat_name: null,
    selected_maj_cat_id: null,
    selected_sub_cat_name: null,
    selected_sub_cat_id: null,    
    clear: () => {
        Model.classes = null,
        Model.task_states = null,
        Model.goal_classes = null,
        Model.major_categories = null,
        Model.sub_categories = null,
        Model.subsub_categories = null,            
        Model.obj = null,
        Model.initialized = false,
        Model.fetched = false,
        Model.selected_maj_cat_name = null,
        Model.selected_maj_cat_id = null,
        Model.selected_sub_cat_name = null,
        Model.selected_sub_cat_id = null        
    },
    initConfig: () => {
        Model.classes = []
        Model.task_states = []
        Model.goal_classes = []

        console.log("Init Config");
        Model.initialized = true;              
        Model.fetchCurrentConfig();
    },
    fetchCurrentConfig: () => {
        m.request({
            url: `/api/v1/users/current/configuration`,
            responseType: "json",
            headers: {
                authorization: auth.getHeaderToken()
            }
        })
            .then(function (obj) {
                Model.classes = obj.classes;
                Model.task_states = obj.task_states;
                Model.goal_classes = obj.goal_classes;
                Model.major_categories = obj.major_categories;
                Model.sub_categories = obj.sub_categories;
                Model.subsub_categories = obj.sub_sub_categories;
                Model.project_lists = obj.project_lists;
                Model.task_lists = obj.task_lists;
                Model.contexts = obj.contexts;                
                Model.obj = obj;

                console.log("Fetched Config.");
                console.log("Obj: " + Model.obj);
                console.log("Major Cat: " + Model.major_categories);
                console.log("Sub Cat: " + Model.sub_categories);
                console.log("Sub-Sub Cat: " + Model.subsub_categories);
                console.log("Proj Lists: " + Model.project_lists);
                console.log("Task Lists: " + Model.task_lists);
                console.log("Contexts: " + Model.contexts);
                Model.fetched = true;
            })
            .catch(err =>
                   auth.retryCall(err, () => Model.fetchCurrentConfig())
                  );        
        // If it was triggered by a click event.
    },
    setSelectedMajCat: (name, id) => {
        if (Model.selected_maj_cat_id === id) return false;
        Model.selected_maj_cat_id = id;
        Model.selected_maj_cat_name = name;
        return true;
    },
    setSelectedSubCat: (name, id) => {
        if (Model.selected_sub_cat_id === id) return false;
        Model.selected_sub_cat_id = id;
        Model.selected_sub_cat_name = name;
        return true;
    },    
    getMajCatName: (id) => {
        for (var i=0; i < Model.major_categories.length; i++) {
            if(Model.major_categories[i].mcr_id === id)
                return Model.major_categories[i].mcr_name;
        }
        return "None"
    },
    getSubCatName: (id) => {
        for (var i=0; i < Model.sub_categories.length; i++) {
            if(Model.sub_categories[i].scr_id === id)
                return Model.sub_categories[i].scr_name;
        }
        return "None"
    }    
};

export default Model;
