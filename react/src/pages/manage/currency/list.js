import React from 'react'
import { defaultProps } from 'recompose'
import _ from 'lodash'
import 'whatwg-fetch'

import { Bs4ModalContainer, Bs4ModalHeaderContainer, Bs4ModalBodyContainer } from '~/bs4/containers/modal'
import { DeafaultBs4CurrencyForm } from './create-form'
import { CurrenciesTable } from './table'

export class ListCurrency extends React.Component {
    constructor (props) {
        super(props)
        this.state = {
            items: [],
            createMode: false,
            editMode: []
        }
        console.warn('Modal is handled using jQuery')
        console.warn('Modal handling is incomplete.  No state update on modal close')
    }
    handleModalState () {
        const modal = $(`#${this.props.id}-create-modal`)
        if (this.state.createMode) {
            modal.modal('show')
        }
    }
    componentDidMount () {
        this.handleModalState()
    }
    componentDidUpdate () {
        this.handleModalState()
    }
    createMode (evt) {
        console.log('Create mode', evt)
        this.setState(s => {
                return _.merge({}, s, { createMode: !s.createMode }) })
    }
    onCreateFormSubmit (e) {
        let fd = new FormData()
        fd.append("sign", e.sign)
        fd.append("code", e.code)
        fd.append("ident", e.ident)
        fd.append("isCrypto", e.isCrypto)
        fd.append("htmlName", e.shortDesc)
        fd.append("htmlLongName", e.longDesc)
        // FIXME: Send language from form
        // fd.append("language", e.language)
        fd.append("language", 'ru')
        fetch(this.props.createApi, {
            method: 'POST',
            body: fd
        }).then(r => r.json())
        .then(j => console.log(j))
    }
    render () {
        const { id, items } = this.props
        const L = this.props.labels
        return (<React.Fragment>
            <div className="row">
                <div className="col">
                    <h1>
                        {L.title}
                        {` `}
                        <i className="fas fa-plus-circle" onClick={e => this.createMode(e)}/>
                    </h1>
                    <i className="fas fa-search d-none"/>
                </div>
            </div>
            <div className="row">
                <div className="col">
                    {items.currencies.length > 0
                        ? <CurrenciesTable items={items}/>
                        : <span>{L.noCurrencies}</span>
                    }
                </div>
            </div>
            <Bs4ModalContainer id={`${id}-create-modal`} fade>
                <Bs4ModalHeaderContainer title={L.modalTitle}/>
                <Bs4ModalBodyContainer>
                    <DeafaultBs4CurrencyForm
                        onSubmit={e => this.onCreateFormSubmit(e)}
                        onDismiss={console.log}
                        />
                </Bs4ModalBodyContainer>
            </Bs4ModalContainer>
        </React.Fragment>)
    }
}

const withDeafaultProps = defaultProps({
    labels: {
        title: 'Валюты',
        modalTitle: 'Новая валюта',
        noCurrencies: 'В системе ещё нет ни одной валюты'
    },
    listApi: '/manage/currency/list',
    createApi: '/manage/currency/create',
    updateApi: '/manage/currnency/update/:id'
})

export default withDeafaultProps(ListCurrency)