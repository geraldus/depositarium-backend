import React from 'react'

import AppNav from './app/nav'
import { collapsedDetails } from './utils'

export default class App extends React.Component {
    currentKnownRoute () {
        const l = window.location
        const { href, pathname } = l
        const { nav } = this.props
        const route = nav.routes.find((e) => e.path === href)
        if (typeof route !== typeof undefined) {
            let r = {  ... route, path: pathname }
            return r
        } else {
            return null
        }
    }
    currentRouteComponent () {
        const route = this.currentKnownRoute ()
        if (route !== null) {
            collapsedDetails('This route is known', route)
            const c = route.component
            const def = c.defaultProps
            return (props) => <route.component { ...def } { ...route.props } { ...props} />
        } else {
            console.warn('Unknown route')
            if (typeof CURRENT_ROUTE_CONFIG != typeof undefined) {
                collapsedDetails('Found current route config', CURRENT_ROUTE_CONFIG)
                const { componentName, props } = CURRENT_ROUTE_CONFIG
                if (_ui.hasOwnProperty(componentName)) {
                    const ui = _ui[componentName]
                    collapsedDetails('Running current route component', ui)
                    const def = _ui[componentName].defaultProps
                    return (propsOverride) => ui({ ... def, ... props, ... propsOverride })
                } else {
                    collapsedDetails(
                        'Current route Component ' + componentName + ' not found in',
                        _ui
                        )
                    console.warn( _ui)
                }
            }
            return () => null
        }
    }

    render () {
        const SelectedComponent = this.currentRouteComponent()
        return (<React.Fragment>
            <AppNav { ... this.props.nav }/>
            <div className="container-fluid">
                <SelectedComponent/>
            </div>
        </React.Fragment>)
    }

}