cardano-cli transaction build \
    --babbage-era \
    --cardano-mode \
	--testnet-magic 1 \
    --tx-in b3fc83e2c5c5dacf472387c4c7880eaa48092790e1287834e4c571156cd8db91#0 \
	--tx-in-script-file ~/Cardano/plutus-vasil/v2-contracts/escrow.plutus \
	--tx-in-datum-file  ~/Cardano/plutus-vasil/v2-contracts/test-datum.json \
	--tx-in-redeemer-file ~/Cardano/plutus-vasil/v2-contracts/test-redeemer.json \
    --tx-in-collateral b3fc83e2c5c5dacf472387c4c7880eaa48092790e1287834e4c571156cd8db91#1 \
    --change-address $(cat ~/Cardano/plutus-vasil/v2-contracts/test-address/payment.addr) \
    --required-signer-hash 27d2fc0931ec6164d8c353bef704edccf526f521f8f7988d8f1606cd \
    --protocol-params-file ~/Cardano/plutus-vasil/v2-contracts/test-address/protocol.json \
	--out-file "unlock-funds.body"

cardano-cli transaction sign \
   --tx-body-file ~/Cardano/plutus-vasil/v2-contracts/src/project-idea/outputs/unlock-funds.body \
   --testnet-magic 1 \
   --signing-key-file ~/Cardano/plutus-vasil/v2-contracts/test-address/payment.skey \
   --out-file ~/Cardano/plutus-vasil/v2-contracts/src/project-idea/outputs/unlock-funds.signed

cardano-cli transaction submit --tx-file ~/Cardano/plutus-vasil/v2-contracts/src/project-idea/outputs/unlock-funds.signed --testnet-magic 1