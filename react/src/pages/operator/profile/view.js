import React from 'react'
import { defaultProps } from 'recompose'

import _ from 'lodash'
import 'whatwg-fetch'


export class ProfileView extends React.Component {
    constructor (props) {
        super(props)
        this.state = {
            balances: {
                rur: {
                    value: 0,
                    paramining: 0
                },
                pzm: {
                    value: 0,
                    paramining: 0
                }
            }
        }
    }
    fetchBalance () {
        const self = this
        // fetch(this.props.apiUrl)
        //     .then(res => res.json())
        //     .then(j => {
        //         let users = j.users
        //         console.log(j, users)
        //         self.setState(s => _.merge({}, s, { list: users }))
        //     })
    }
    componentDidMount () {
        this.fetchBalance()
    }
    render () {
        const L = this.props.labels
        const { balances } = this.state
        return (<React.Fragment>
            <div className="h3">
                {L.rurBalance}:
            </div>
            <div className="h1">
                {balances.rur.value}&nbsp;₽
            </div>
            <div>{L.paramining} {balances.rur.paramining}</div>
            <div>
                <a href="#">{L.details}</a>
            </div>
            <div className="h3">
                {L.pzmBalance}:
            </div>
            <div className="h1">
                {balances.pzm.value}&nbsp;PZM
            </div>
            <div>{L.paramining} {balances.pzm.paramining}</div>
            <div>
                <a href="#">{L.details}</a>
            </div>
            <div className="row">
                <div className="col-6 d-flex justify-content-center">
                    <a href="#" className="mx-auto">Пополнение</a>
                </div>
                <div className="col-6 d-flex justify-content-center">
                    <a href="#" className="mx-auto">Вывод</a>
                </div>
            </div>
            <div className="row">
                <div className="col">
                    <div className="h4">{L.history}</div>
                </div>
            </div>
        </React.Fragment>)
    }
}

const withDeafaultProps = defaultProps({
    labels: {
        rurBalance: 'Баланс ₽',
        pzmBalance: 'Баланс PRIZM',
        paramining: 'Парамайнинг',
        details: 'Подробнее',
        history: 'История операций'
    }
})

export default withDeafaultProps(ProfileView)