use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    iter,
};

#[derive(Debug, Default, Clone)]
pub struct Place<PlaceId, Analysis> {
    pub initial_analysis: Analysis,
    pub flows_from: Vec<PlaceId>,
}

pub trait LatticeMerge: Default {
    fn lattice_merge(iterator: impl Iterator<Item = Self>) -> Self;
}

impl<PlaceId: Hash + Eq + Default, Analysis: LatticeMerge> LatticeMerge
    for Place<PlaceId, Analysis>
{
    fn lattice_merge(mut iterator: impl Iterator<Item = Self>) -> Self {
        let Some(first_place) = iterator.next() else {
            return Place::default();
        };
        let mut flows_from = HashSet::new();
        flows_from.extend(first_place.flows_from);
        let initial_analysis = Analysis::lattice_merge(
            iterator
                .map(|place| {
                    flows_from.extend(place.flows_from);
                    place.initial_analysis
                })
                .chain(iter::once(first_place.initial_analysis)),
        );
        Place {
            initial_analysis,
            flows_from: flows_from.into_iter().collect(),
        }
    }
}

pub fn analyze_flow<PlaceId: Hash + Eq + Clone, Analysis: LatticeMerge + Clone>(
    places: &HashMap<PlaceId, Place<PlaceId, Analysis>>,
) -> HashMap<PlaceId, Analysis> {
    let mut visited = HashSet::new();
    let mut results = HashMap::new();
    for place_id in places.keys() {
        compute_analysis_for_place(place_id, places, &mut visited, &mut results);
    }
    results
}

fn compute_analysis_for_place<PlaceId: Clone + Hash + Eq, Analysis: Clone + LatticeMerge>(
    place_id: &PlaceId,
    places: &HashMap<PlaceId, Place<PlaceId, Analysis>>,
    visited: &mut HashSet<PlaceId>,
    results: &mut HashMap<PlaceId, Analysis>,
) -> Analysis {
    if let Some(existing) = results.get(place_id) {
        return existing.clone();
    }

    let place = places.get(place_id).unwrap();

    if !visited.insert(place_id.clone()) {
        // Don't recurse through loops.
        return place.initial_analysis.clone();
    }

    let result = LatticeMerge::lattice_merge(
        iter::once(place.initial_analysis.clone()).chain(
            place
                .flows_from
                .iter()
                .map(|other_id| compute_analysis_for_place(other_id, places, visited, results)),
        ),
    );
    results.insert(place_id.clone(), result.clone());
    result
}
