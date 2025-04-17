use std::collections::HashMap;
use crate::analysis::rule::SemanticRule;

pub struct RuleRegistry {
    rules: HashMap<String, Box<dyn SemanticRule>>,
}

impl RuleRegistry {
    pub fn new() -> Self {
        Self {
            rules: HashMap::new(),
        }
    }

    pub fn register<R: SemanticRule + 'static>(&mut self, rule: R) {
        let rule_id = rule.id().to_string();
        self.rules.insert(rule_id, Box::new(rule));
    }

    pub fn get_rule(&self, rule_id: &str) -> Option<&dyn SemanticRule> {
        self.rules.get(rule_id).map(|r| r.as_ref())
    }

    pub fn get_all_rules(&self) -> Vec<&dyn SemanticRule> {
        self.rules.values().map(|r| r.as_ref()).collect()
    }
}