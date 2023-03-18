cardano-cli transaction sign \
	--signing-key-file ~/Cardano/plutus-vasil/plutus-vasil-v2/test-addresses/01.skey \
	--testnet-magic 1 \
	--tx-body-file tokenMint.unsigned \
	--out-file tokenMint.signed

cardano-cli transaction submit --tx-file tokenMint.signed --testnet-magic 1 
